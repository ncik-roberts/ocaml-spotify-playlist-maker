open Core
open Async

let cleanup1 str =
  str
  |> String.map ~f:(fun c -> if Char.is_alphanum c then c else ' ')
  |> String.split ~on:' '
  |> List.filter ~f:(Fn.non String.is_empty)
  |> String.concat ~sep:" "

let cleanup2 str =
  str
  |> String.filter ~f:(fun c -> Char.is_alphanum c || c = ' ')

let queries { Npr.Song.album; title; artist; start_time = _ } =
  let open List.Let_syntax in
  let all x = [ x; cleanup1 x; cleanup2 x ] in
  let title_artists =
    let%bind title = all title in
    let%bind artist = all artist in
    Spotify.Query.create ~album:None ~track:(Some title) ~artist:(Some artist)
    |> return
  in
  let title_albums =
    match album with
    | None -> []
    | Some album ->
      let%bind title = all title in
      let%bind album = all album in
      Spotify.Query.create ~album:(Some album) ~track:(Some title) ~artist:None
      |> return
  in
  List.dedup_and_sort ~compare:Spotify.Query.compare (title_artists @ title_albums)

let tracks
    (songs : Npr.Song.t Or_error.t Pipe.Reader.t)
    ~(client : Spotify_async_client.t)
  =
  let num_failures = ref 0 in
  Pipe.create_reader ~close_on_exception:true (fun writer ->
    Pipe.iter songs ~f:(fun song_or_error ->
      match song_or_error with
      | Error e ->
        error_s [%message "NPR pipe returned with error" (e : Error.t)]
        |> Pipe.write writer
      | Ok song ->
        let rec loop = function
          | [] -> Ok (`Skipping song) |> Pipe.write writer
          | query :: queries ->
            (match%bind Spotify_async_client.search_track client ~query with
             | Ok (Some track) -> Pipe.write writer (Ok (`Found (song, track)))
             | Ok None -> loop queries
             | Error e ->
               if !num_failures > 3
               then
                 error_s [%message "Too many failures" (e : Error.t)]
                 |> Pipe.write writer
               else (
                 print_endline "Waiting 30 sec...";
                 let%bind () = after (sec 30.) in
                 loop (query :: queries)))
        in
        loop (queries song)))
