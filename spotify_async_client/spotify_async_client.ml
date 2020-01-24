open Core
open Async

module Config = struct
  type t =
    { port : int
          (** A free port on localhost. This port will be used by this client
       *  to listen for the client authorization flow token.
       *)
    ; credentials : Spotify.Credentials.t
    ; user_id : string
    ; debug : bool
    }
    [@@deriving yojson]
end

module Config_with_refresh_token = struct
  type t =
    { config : Config.t
    ; refresh_token : Spotify.Authorization_code_flow.Refresh_token.t
    }
  [@@deriving yojson { exn = true }]
end

module Current_access_token = struct
  type t =
    { access_token : Spotify.Authorization_code_flow.Access_token.t
    ; refresh_token : Spotify.Authorization_code_flow.Refresh_token.t
    ; when_to_refresh : Time_ns.t
    }
end

type t =
  { config : Config.t
  ; cohttp_request_async_client : Cohttp_request_async_client.t
  ; mutable current_access_token : Current_access_token.t
  }
  [@@deriving fields]


let ignore_response_code
    (deferred : ('a, Cohttp_request_async_client.Request_error.t) Result.t Deferred.t)
    : 'a Or_error.t Deferred.t
  =
  match%map deferred with
  | Ok x -> Ok x
  | Error { response_code = _; error } -> Error error
;;

let create_client (config : Config.t) =
  let debug_mode : Cohttp_request_async_client.Debug_mode.t option =
    if config.debug then Some { print_requests = true; print_responses = true } else None
  in
  Cohttp_request_async_client.create ~debug_mode

let current_access_token
    { Spotify.Authorization_code_flow.access_token; refresh_token; expires_in }
  =
  let when_to_refresh =
    Time_ns.add (Time_ns.now ()) (Time_ns.Span.of_int_sec expires_in)
  in
  { Current_access_token.access_token; refresh_token; when_to_refresh }

(* A human must visit the returned URI to authorize the Spotify app.
 * Only then will the returned deferred become determined.
 *)
let create ?write_config_to (config : Config.t) ~scopes =
  let open Deferred.Or_error.Let_syntax in
  let cohttp_request_async_client = create_client config in
  let uri, authorization_code_deferred =
    Spotify_authorization_code_server.listen_for_authorization_code
      ~client_id:(Spotify.Credentials.client_id config.credentials)
      ~port:config.port
      ~scopes
  in
  let deferred =
    let%bind authorization_code = authorization_code_deferred in
    let%bind access_token =
      Cohttp_request_async_client.request
        cohttp_request_async_client
        (Spotify.Authorization_code_flow.get_access_token
           authorization_code
           ~credentials:config.credentials)
      |> ignore_response_code
    in
    Option.iter write_config_to ~f:(fun file ->
      Config_with_refresh_token.to_yojson { config; refresh_token = access_token.refresh_token }
      |> Yojson.Safe.to_file file);
    { config
    ; cohttp_request_async_client
    ; current_access_token = current_access_token access_token
    }
    |> return
  in
  uri, deferred
;;

let create_with_refresh_token (config : Config.t) refresh_token =
  let open Deferred.Or_error.Let_syntax in
  let cohttp_request_async_client = create_client config in
  let%bind access_token =
    Cohttp_request_async_client.request
      cohttp_request_async_client
      (Spotify.Authorization_code_flow.refresh_access_token
         refresh_token
         ~credentials:config.credentials)
    |> ignore_response_code
  in
  { config
  ; cohttp_request_async_client
  ; current_access_token = current_access_token access_token
  }
  |> return

let wrap_in_current_access_token t request =
  let rec loop ~num_failures =
    match%bind
      Cohttp_request_async_client.request
        t.cohttp_request_async_client
        (request ~access_token:t.current_access_token.access_token)
    with
    | Ok result -> return (Ok result)
    | Error { response_code; error } ->
      if num_failures > 2
      then return (Error error)
      else (
        (* https://developer.spotify.com/documentation/web-api/#response-status-codes *)
        match response_code with
        | 429 ->
          (* Too many requests *)
          let rate_limit_sec = 10 in
          if t.config.debug then printf "Waiting %d sec...\n" rate_limit_sec;
          let%bind () = after (Time.Span.of_int_sec rate_limit_sec) in
          loop ~num_failures:(num_failures + 1)
        | 401 ->
          (* Unauthorized *)
          let time_before_we_need_to_refresh =
            Time_ns.diff t.current_access_token.when_to_refresh (Time_ns.now ())
          in
          if Time_ns.Span.( > )
               time_before_we_need_to_refresh
               (Time_ns.Span.of_int_sec 10)
          then return (Error error)
          else
            let open Deferred.Or_error.Let_syntax in
            let%bind { Spotify.Authorization_code_flow.access_token
                     ; refresh_token
                     ; expires_in
                     }
              =
              Spotify.Authorization_code_flow.refresh_access_token
                ~credentials:t.config.credentials
                t.current_access_token.refresh_token
              |> Cohttp_request_async_client.request t.cohttp_request_async_client
              |> ignore_response_code
            in
            let when_to_refresh =
              Time_ns.add (Time_ns.now ()) (Time_ns.Span.of_int_sec expires_in)
            in
            t.current_access_token <- { access_token; refresh_token; when_to_refresh };
            loop ~num_failures:(num_failures + 1)
        | _ -> return (Error error))
  in
  loop ~num_failures:0
;;

let make_playlist t ~kind ~name =
  wrap_in_current_access_token
    t
    (Spotify.make_playlist ~kind ~user_id:t.config.user_id ~name)
;;

let all_tracks_in_playlist t ~playlist =
  let limit = 100 in
  let get_tracks ~offset =
    wrap_in_current_access_token t (Spotify.lookup_playlist ~offset ~limit () ~playlist)
  in
  let open Deferred.Or_error.Let_syntax in
  let%bind tracks = get_tracks ~offset:0 in
  if tracks.total <= limit
  then return tracks.items
  else (
    let rec loop offset ~state =
      if offset >= tracks.total
      then return (List.rev state)
      else (
        let%bind tracks = get_tracks ~offset in
        loop (offset + limit) ~state:(List.rev tracks.items @ state))
    in
    loop limit ~state:(List.rev tracks.items))
;;

let add_to_playlist t ~playlist ~tracks =
  wrap_in_current_access_token t (Spotify.add_to_playlist ~playlist ~tracks)
;;

let search_track t ~query =
  let open Deferred.Or_error.Let_syntax in
  let%bind tracks =
    wrap_in_current_access_token t (Spotify.search_tracks ~limit:1 ~query ())
  in
  match tracks.items with
  | [] -> return None
  | track :: _ -> return (Some track)
;;

module Param = struct

let credentials ~credentials_file =
  match In_channel.read_lines credentials_file with
  | [ client_id; client_secret ] -> Spotify.Credentials.create ~client_id ~client_secret
  | _ -> failwith ("Invalid file " ^ credentials_file)
;;

  let config_param =
    let%map_open.Command.Let_syntax () = return ()
    and port =
      flag
        "--port"
        (optional_with_default 8888 int)
        ~doc:"8888 port to use for fetching authorization code"
    and credentials_file =
      flag
        "--credentials-file"
        (optional_with_default "credentials.txt" string)
        ~doc:"credentials.txt the credentials file (first line is client ID, second line is client secret)"
    and user_id =
      flag
        "--user-id"
        (optional_with_default "ncik_roberts" string)
        ~doc:"ncik_roberts Spotify user id"
    and debug = flag "--debug" no_arg ~doc:" debug mode (print a lot)"
    in
    { Config.port; credentials = credentials ~credentials_file; user_id; debug }

  let config_of_config_file_param =
    let%map_open.Command.Let_syntax () = return ()
    and config_file = anon Command.Anons.("FILE" %: string)
    in
    Yojson.Safe.from_file config_file
    |> Config_with_refresh_token.of_yojson_exn

  let param_of_config_param config_param =
    let%map_open.Command.Let_syntax () = return ()
    and config = config_param
    and write_config_to =
      flag
        "--write-config-to"
        (optional string)
        ~doc:"FILE if present, write the configured options to this config file"
    in
    fun ~scopes ->
      let uri, deferred = create ?write_config_to config ~scopes in
      uri, deferred >>| ok_exn

  let param = param_of_config_param config_param

  let from_config_file =
    let%map_open.Command.Let_syntax () = return ()
    and config = config_of_config_file_param
    in
    create_with_refresh_token config.config config.refresh_token >>| ok_exn

  let from_config_file_refreshing_refresh_token =
    param_of_config_param (
      let%map_open.Command.Let_syntax () = return ()
      and config = config_of_config_file_param
      in
      config.config)

end
