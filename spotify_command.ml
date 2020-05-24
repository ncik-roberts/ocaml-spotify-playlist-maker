open Core
open Async

let make_time date hour ~zone =
  let time = Time_ns.Ofday.create ~hr:hour () in
  Time_ns.of_date_ofday ~zone date time
;;

let within_the_last time ~span =
  Time_ns.Span.( < ) (Time_ns.diff (Time_ns.now ()) time) span
;;

let prompt_in_web_browser_for_authorization uri =
  match%map Process.run ~prog:"xdg-open" ~args:[ Uri.to_string uri ] () with
  | Error _ as error -> error
  | Ok (_output : string) -> Ok ()
;;

let generate_playlist_name ~begin_ ~end_ ~begin_time ~end_time =
  let suffix =
    if Date.equal begin_ end_
    then
      sprintf
        "%s, from %d:00 til %d:59"
        (Date.to_string_american begin_)
        begin_time
        (end_time - 1)
    else
      sprintf
        "from %s at %d:00, til %s at %d:59"
        (Date.to_string_american begin_)
        begin_time
        (Date.to_string_american end_)
        end_time
  in
  sprintf "WYEP Playlist %s" suffix
;;

let command =
  Command.async
    ~summary:"Print the spotify IDs to stdout"
    (let%map_open.Command begin_ =
       flag
         ~aliases:[ "b" ]
         "--begin"
         (optional date)
         ~doc:(sprintf "YYYY-MM-DD Start date (incl)")
     and begin_time =
       flag
         ~aliases:[ "bh" ]
         "--begin-hour"
         (optional_with_default 0 int)
         ~doc:"0 Start hour (military; incl)"
     and end_ =
       flag
         ~aliases:[ "e" ]
         "--end"
         (optional date)
         ~doc:(sprintf "YYYY-MM-DD End date (incl)")
     and end_time =
       flag
         ~aliases:[ "eh" ]
         "--end-hour"
         (optional int)
         ~doc:"24 End hour (military; incl)"
     and playlist_uri =
       flag
         "--playlist-uri"
         (optional string)
         ~doc:"playlist_uri The playlist to append to (creates a new playlist if omitted)"
     and playlist_name =
       flag
         "--playlist-name"
         (optional string)
         ~doc:
           "playlist_name The name for the playlist. Auto-generated based on the date if \
            omitted. Cannot be combined with --playlist-id."
     and batch_size =
       flag
         "--batch-size"
         (optional_with_default 20 int)
         ~doc:"batch size of tracks to add to playlist"
     and station_id =
       flag
         "--station-id"
         (optional_with_default "50e451b6a93e91ee0a00028e" string)
         ~doc:" station id to query"
     and client_creator = Spotify_async_client.Param.param in
     fun () ->
       let open Deferred.Let_syntax in
       let zone = force Time_ns.Zone.local in
       let today = Date.today ~zone in
       let begin_ = Option.value begin_ ~default:today in
       let end_ = Option.value end_ ~default:today in
       let end_time = Option.value end_time ~default:24 in
       let from = make_time ~zone begin_ begin_time in
       let until = make_time ~zone end_ end_time in
       let scopes =
         match playlist_uri with
         | Some _ ->
           [ `Playlist_modify_public; `Playlist_modify_private; `Playlist_read_private ]
         | None -> [ `Playlist_modify_private ]
       in
       let uri, client_deferred = client_creator ~scopes in
       let%bind () = prompt_in_web_browser_for_authorization uri >>| ok_exn in
       let%bind client = client_deferred in
       let config = Spotify_async_client.config client in
       let npr = Npr.create ~debug_mode:config.debug ~station_id in
       let%bind playlist, uris_already_added_to_playlist =
         match
           playlist_uri |> Option.map ~f:(fun uri -> Spotify.Playlist.parse (`Uri uri))
         with
         | Some (Error `Invalid_playlist) ->
           raise_s [%message "Invalid playlist uri" (playlist_uri : string option)]
         | Some (Ok playlist) ->
           let%bind items_in_playlist =
             Spotify_async_client.all_tracks_in_playlist client ~playlist >>| ok_exn
           in
           let uris_in_playlist = List.map items_in_playlist ~f:(fun x -> x.uri) in
           return (playlist, String.Set.of_list uris_in_playlist)
         | None ->
           let playlist_name =
             match playlist_name with
             | None -> generate_playlist_name ~begin_ ~end_ ~begin_time ~end_time
             | Some playlist_name -> playlist_name
           in
           let%bind playlist =
             Spotify_async_client.make_playlist client ~kind:`Private ~name:playlist_name
             >>| ok_exn
           in
           return (playlist, String.Set.empty)
       in
       let songs = Npr.lookup_songs npr ~from ~until in
       let tracks = Generate_spotify_tracks.tracks ~client songs in
       let module State = struct
         type t = { batch : Spotify.Track.t list }
       end
       in
       let send_batch (state : State.t) =
         let batch =
           List.rev state.batch
           |> List.filter ~f:(fun { Spotify.Track.uri; _ } ->
                  not (Set.mem uris_already_added_to_playlist uri))
         in
         match batch with
         | [] -> return ()
         | tracks ->
           Spotify_async_client.add_to_playlist client ~playlist ~tracks >>| ok_exn
       in
       let add_to_batch ~(state : State.t) song track =
         let state = { State.batch = track :: state.batch } in
         (* Always send the batch if we've gotten to the batch size, or if we're
          * "pretty close to now" (i.e. we're streaming songs directly from the NPR
          * playlist to the spotify playlist).
          *)
         if List.length state.batch >= batch_size
            || within_the_last song.Npr.Song.start_time ~span:(Time_ns.Span.of_min 8.)
         then (
           let%bind () = send_batch state in
           return { State.batch = [] })
         else return state
       in
       let%bind final_state =
         Pipe.fold tracks ~init:{ State.batch = [] } ~f:(fun state track ->
             match ok_exn track with
             | `Skipping song ->
               printf
                 "Skipping \"%s\" (artist=\"%s\"; album=\"%s\")\n"
                 song.title
                 song.artist
                 (Option.value ~default:"" song.album);
               return state
             | `Found (song, track) -> add_to_batch ~state song track)
       in
       send_batch final_state)
;;
