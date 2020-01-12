open Core
open Async

let credentials_file = "credentials.txt"

let within_the_last time ~span =
  Time_ns.Span.(<) (Time_ns.diff (Time_ns.now ()) time) span

let command =
  Command.async
    ~summary:"Print the spotify IDs to stdout"
    (let open Command.Param in
     let%map.Command.Let_syntax begin_ =
       flag ~aliases:["b"] "--begin"
         (optional date)
         ~doc:(sprintf "YYYY-MM-DD Start date (incl)")
     and begin_time =
       flag ~aliases:["bh"] "--begin-hour"
         (optional_with_default 0 int)
         ~doc:"0 Start hour (military; incl)"
     and debug = flag "--debug" no_arg ~doc:" Turn on debug mode"
     and end_ =
       flag ~aliases:["e"] "--end"
         (optional date)
         ~doc:(sprintf "YYYY-MM-DD End date (incl)")
     and end_time =
       flag ~aliases:["eh"] "--end-hour"
         (optional int)
         ~doc:"24 End hour (military; incl)"
     and user_id =
       flag ~aliases:["u"] "--user-id"
         (optional_with_default "ncik_roberts" string)
         ~doc:"ncik_roberts Spotify user id"
     and playlist_id =
       flag "--playlist-id"
         (optional string)
         ~doc:"playlist_id The playlist to append to (creates a new playlist if omitted)"
     and batch_size =
       flag "--batch-size"
         (optional_with_default 20 int)
         ~doc:"batch size of tracks to add to playlist"
     and station_id =
       flag "--station-id"
         (optional_with_default "50e451b6a93e91ee0a00028e" string)
         ~doc:" station id to query"
     and port =
       flag "--port"
         (optional_with_default 8888 int)
         ~doc:"8888 port to use for fetching authorization code"
     and credentials_file =
       flag ~aliases:["c"] "--credentials-file"
          (optional_with_default credentials_file string)
          ~doc:"credentials.txt the credentials file"
     in
     fun () ->
       let open Deferred.Let_syntax in
       Npr.set_debug_mode debug;
       Spotify.set_debug_mode debug;
       let zone = force Time_ns.Zone.local in
       let today = Date.today ~zone in
       let begin_ = Option.value begin_ ~default:today in
       let make_time date hour =
         let time = Time_ns.Ofday.create ~hr:hour () in
         Time_ns.of_date_ofday ~zone date time
       in
       let end_ = Option.value end_ ~default:today in
       let end_time = Option.value end_time ~default:24 in
       let `Client_id client_id, `Client_secret client_secret =
         match In_channel.read_lines credentials_file with
         | [ client_id; client_secret ] ->
           `Client_id client_id, `Client_secret client_secret
         | _ -> failwith ("Invalid file " ^ credentials_file)
       in
       let npr = Npr.create ~station_id in
       let from = make_time begin_ begin_time in
       let until = make_time end_ end_time in
       let credentials = Spotify.Credentials.create ~client_id ~client_secret in
       let scopes =
         match playlist_id with
         | Some _ ->
           [ `Playlist_modify_public
           ; `Playlist_modify_private
           ; `Playlist_read_private
           ]
         | None -> [ `Playlist_modify_private ]
       in
       let%bind authorization_code =
          Spotify_authorization_code_fetcher.fetch_authorization_code
            ~client_id
            ~port
            ~scopes
          >>| ok_exn
        in
        let%bind client_credentials_flow =
          Spotify.Client_credentials_flow.get_access_token ~credentials
          >>| ok_exn
        in
        let%bind authorization_code_flow =
          Spotify.Authorization_code_flow.get_access_token
            authorization_code
            ~credentials
          >>| ok_exn
        in
        let { Spotify.Authorization_code_flow.access_token; refresh_token; expires_in } =
          authorization_code_flow
        in
        let when_to_refresh =
          Time_ns.add (Time_ns.now ()) (Time_ns.Span.of_int_sec expires_in)
        in
        let%bind playlist, uris_in_playlist =
          match playlist_id with
          | Some playlist ->
            let playlist = Spotify.Playlist.of_id playlist in
            let%bind uris_in_playlist =
              Spotify.lookup_playlist
                ~access_token
                ~playlist
              >>| ok_exn
              >>| List.map ~f:(fun x -> x.Spotify.Track.uri)
            in
            return (playlist, String.Set.of_list uris_in_playlist)
          | None ->
            let playlist_name =
              let suffix =
                if Date.equal begin_ end_
                then sprintf "%s, from %d:00 til %d:59"
                  (Date.to_string_american begin_)
                  begin_time
                  (end_time-1)
                else sprintf "from %s at %d:00, til %s at %d:59"
                  (Date.to_string_american begin_)
                  begin_time
                  (Date.to_string_american end_)
                  end_time
              in
              sprintf "WYEP Playlist %s" suffix
            in
            let%bind playlist =
              Spotify.make_playlist
                ~kind:`Private
                ~access_token
                ~user_id
                ~name:playlist_name
              >>| ok_exn
            in
            return (playlist, String.Set.empty)
        in
        let songs = Npr.lookup_songs npr ~from ~until in
        let tracks =
          Generate_spotify_tracks.tracks
            songs
            ~access_token:client_credentials_flow.access_token
        in
        let
          module State = struct
            type t =
              { to_write : Spotify.Track.t list
              ; access_token : Spotify.Authorization_code_flow.Access_token.t
              ; refresh_token : Spotify.Authorization_code_flow.Refresh_token.t
              ; when_to_refresh : Time_ns.t
              }

            let init =
              { to_write = []
              ; access_token
              ; refresh_token
              ; when_to_refresh
              }
          end
        in
        let rec send_remaining ?(num_failures = 0) (state : State.t) =
          match state.to_write with
          | [] -> return state
          | tracks ->
            let tracks =
              List.rev tracks
              |> List.filter ~f:(fun { Spotify.Track.uri; _ } ->
                    not (Set.mem uris_in_playlist uri))
            in
            match%bind Spotify.add_to_playlist ~access_token ~playlist ~tracks with
            | Error e ->
              if num_failures > 3 then Error.raise e;
              if Time_ns.(<) (Time_ns.now ()) (state.when_to_refresh)
              then (
                print_endline "Waiting 30 sec...";
                let%bind () = after (Time.Span.of_int_sec 30) in
                send_remaining ~num_failures:(num_failures + 1) state)
              else
                let%bind
                  { Spotify.Authorization_code_flow.access_token
                  ; refresh_token
                  ; expires_in
                  }
                =
                  Spotify.Authorization_code_flow.refresh_access_token
                    ~credentials
                    state.refresh_token
                  >>| ok_exn
                in
                let when_to_refresh =
                  Time_ns.add (Time_ns.now ()) (Time_ns.Span.of_int_sec expires_in)
                in
                send_remaining
                  ~num_failures:(num_failures + 1)
                  { to_write = state.to_write
                  ; access_token
                  ; refresh_token
                  ; when_to_refresh
                  }
            | Ok () -> return { state with to_write = [] }
        in
        let digest ~(state : State.t) song track =
          let state = { state with to_write = track :: state.to_write } in
          if List.length state.to_write >= batch_size
            || within_the_last song.Npr.Song.start_time ~span:(Time_ns.Span.of_min 8.)
          then send_remaining state
          else return state
        in
        let%bind final_state =
          Pipe.fold tracks ~init:State.init ~f:(fun state track ->
            match track with
            | Error e -> Error.raise e
            | Ok (`Skipping song) ->
              printf
                "Skipping \"%s\" (artist=\"%s\"; album=\"%s\")\n"
                song.title
                song.artist
                (Option.value ~default:"" song.album);
              return state
            | Ok (`Found (song, track)) -> digest ~state song track)
        in
        send_remaining final_state |> Deferred.ignore_m)
