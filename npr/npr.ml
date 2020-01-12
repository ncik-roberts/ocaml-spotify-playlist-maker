open Core
open Async

let base_uri station_id =
  sprintf
    "https://api.composer.nprstations.org/v1/widget/%s/playlist"
    station_id
  |> Uri.of_string

type t = { station_id : string } [@@deriving sexp_of]

module Song = struct
  module External = struct
    type t =
     { title : string
     ; artist : string
     ; album : string option
     ; start_time : Time_ns.t
     }
     [@@deriving sexp_of]
  end

  module Internal = struct
    type t =
      { id : string
      ; trackName : string
      ; artistName : string
      ; _start_time : string
      ; _duration : int
      ; collectionName : string option [@default None]
      }
      [@@deriving sexp_of, yojson { strict = false }]
  end

  include External
end

module Playlist = struct
  type playlist = { playlist : Song.Internal.t list } [@@deriving yojson { strict = false }]
  type t = { playlist : playlist list } [@@deriving yojson { strict = false }]
end


let create ~station_id = { station_id }

let query t time =
  let date, time_ofday =
    Time_ns.to_date_ofday time ~zone:(force Time_ns.Zone.local)
  in
  let year = Date.year date in
  let month = Date.month date |> Month.to_int in
  let day = Date.day date in
  let parts = Time_ns.Ofday.to_parts time_ofday in
  let uri =
    Uri.add_query_params (base_uri t.station_id)
      [ "datestamp", [ sprintf "%04d-%02d-%02d" year month day ]
      ; "order", [ "1" ]
      ; "time", [ sprintf "%02d:%02d" parts.hr parts.min ]
      ]
  in
  { Cohttp_request.uri
  ; request_type = Get
  ; headers = Cohttp.Header.init ()
  ; response_of_yojson = (module Playlist)
  }
  |> Cohttp_request.map ~f:(fun { Playlist.playlist } ->
      List.concat_map playlist ~f:(fun { playlist } -> playlist))

let externalize_song song ~start_time =
  { Song.External.title = song.Song.Internal.trackName
  ; artist = song.artistName
  ; album = song.collectionName
  ; start_time
  }

let parse_time_ns_of_start_time start_time =
  Or_error.try_with (fun () ->
    Scanf.sscanf start_time "%d-%d-%d %d:%d:%d"
      (fun month day year hour minute second ->
        let month = Month.of_int_exn month in
        let date = Date.create_exn ~y:year ~m:month ~d:day in
        Time_ns.of_date_ofday
          ~zone:(force Time_ns.Zone.local)
          date
          (Time_ns.Ofday.create ~hr:hour ~min:minute ~sec:second ())))

module Backoff : sig
  type t
  val reset : t
  val at : Time_ns.t -> t
  val increase : t -> t
  val wait : t -> unit Deferred.t
end = struct
  type t =
    | Wait_until_this_time of Time_ns.t
    | Wait_this_long of Time_ns.Span.t

  let lowest_backoff = Time_ns.Span.of_int_ms 500

  let reset = Wait_this_long lowest_backoff

  let at time = Wait_until_this_time time

  let increase = function
    | Wait_until_this_time time ->
      Wait_until_this_time (Time_ns.add time (Time_ns.Span.of_int_sec 30))
    | Wait_this_long span -> Wait_this_long (Time_ns.Span.scale span 1.5)

  let wait = function
    | Wait_until_this_time this_time ->
      Clock_ns.at
        (Time_ns.min this_time (Time_ns.add (Time_ns.now ()) (Time_ns.Span.of_min 10.)))
    | Wait_this_long this_long -> Clock_ns.after this_long

end

let add_some_time time =
  Time_ns.min
    (Time_ns.now ())
    (Time_ns.add time (Time_ns.Span.of_min 30.))

let lookup_until_done t writer ~from ~done_ ~in_range =
  let rec loop time last_time song_ids backoff =
    if done_ time then Deferred.unit
    else
      let%bind () = Backoff.wait backoff in
      let%bind songs = Cohttp_request_async.request (query t time) in
      match songs with
      | Error e -> Pipe.write_if_open writer (Error e)
      | Ok [] -> loop (add_some_time time) time song_ids (Backoff.increase backoff)
      | Ok songs ->
          let new_songs =
            List.map songs ~f:(fun song ->
              let open Or_error.Let_syntax in
              let%map start_time = parse_time_ns_of_start_time song._start_time in
              start_time, song)
            |> Or_error.combine_errors
          in
          match new_songs with
          | Error error -> Pipe.write_if_open writer (Error error)
          | Ok new_songs ->
            let (last_song_time, last_song) = List.last_exn new_songs in
            let new_time =
              Time_ns.add last_song_time (Time_ns.Span.of_int_ms last_song._duration)
            in
            let new_time =
              let new_time =
                if Time_ns.compare new_time last_time < 0 then last_time
                else new_time
              in
              if Time_ns.compare last_time new_time <> 0
              then new_time
              else add_some_time last_time
            in
            match
              List.filter new_songs
                ~f:(fun (start_time, song) ->
                  not (Set.mem song_ids song.id) && in_range start_time)
            with
            | [] -> loop new_time time song_ids (Backoff.increase backoff)
            | songs_to_write ->
              let%bind () =
                Deferred.List.iter songs_to_write
                  ~f:(fun (start_time, song) ->
                    Pipe.write_if_open writer (Ok (externalize_song ~start_time song)))
              in
              let new_song_ids =
                List.map songs_to_write ~f:(fun (_, new_song) -> new_song.id)
                |> String.Set.of_list
              in
              let song_ids' = Set.union song_ids new_song_ids in
              loop new_time time song_ids' (Backoff.at new_time)
  in
  loop from from String.Set.empty Backoff.reset

let lookup_songs t ~from ~until =
  if Time_ns.(>) from until
  then raise_s [%message "from exceeds until" (from : Time_ns.t) (until : Time_ns.t)]
  else
    Pipe.create_reader ~close_on_exception:true (fun writer ->
      lookup_until_done t writer
        ~from
        ~done_:(fun time -> Time_ns.(time > until))
        ~in_range:(fun time ->
          Time_ns.equal time (Time_ns.clamp_exn time ~min:from ~max:until)))
