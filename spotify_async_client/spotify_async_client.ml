open Core
open Async

module Config = struct
  type t =
    { port : int
      (** A free port on localhost. This port will be used by this client
       *  to listen for the client authorization flow token.
       *)
    ; credentials : Spotify.Credentials.t
    ; scopes : Spotify.Scope.t list
    ; user_id : string
    }
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
  ; mutable current_access_token : Current_access_token.t
  }


(* A human must visit the returned URI to authorize the Spotify app.
 * Only then will the returned deferred become determined.
 *)
let create (config : Config.t) =
  let open Deferred.Or_error.Let_syntax in
  let uri, authorization_code_deferred =
    Spotify_authorization_code_server.listen_for_authorization_code
      ~client_id:(Spotify.Credentials.client_id config.credentials)
      ~port:config.port
      ~scopes:config.scopes
  in
  let deferred =
    let%bind authorization_code = authorization_code_deferred in
    let%bind
      { Spotify.Authorization_code_flow.access_token
      ; refresh_token
      ; expires_in
      }
    =
      Cohttp_request_async.request
        (Spotify.Authorization_code_flow.get_access_token
          authorization_code
          ~credentials:config.credentials)
    in
    let when_to_refresh =
      Time_ns.add (Time_ns.now ()) (Time_ns.Span.of_int_sec expires_in)
    in
    { config; current_access_token = { access_token; refresh_token; when_to_refresh } }
    |> return
  in
  uri, deferred

let wrap_in_current_access_token t request =
  Cohttp_request_async.request
    (request ~access_token:t.current_access_token.access_token)

let make_playlist t ~kind ~name =
  wrap_in_current_access_token t (
    Spotify.make_playlist ~kind ~user_id:t.config.user_id ~name)

let all_tracks_in_playlist t ~playlist =
  let limit = 100 in
  let get_tracks ~offset =
    wrap_in_current_access_token t (
      Spotify.lookup_playlist ~offset ~limit () ~playlist)
  in
  let open Deferred.Or_error.Let_syntax in
  let%bind tracks = get_tracks ~offset:0 in
  if tracks.total <= 100 then return tracks.items
  else
    let rec loop offset ~state =
      if offset >= tracks.total then return (List.rev state)
      else
        let%bind tracks = get_tracks ~offset in
        loop (offset + limit) ~state:(List.rev tracks.items @ state)
    in
    loop limit ~state:(List.rev tracks.items)

let add_to_playlist t ~playlist ~tracks =
  wrap_in_current_access_token t (
    Spotify.add_to_playlist ~playlist ~tracks)

let search_track t ~query =
  let open Deferred.Or_error.Let_syntax in
  let%bind tracks =
    wrap_in_current_access_token t (
      Spotify.search_tracks ~limit:1 ~query ())
  in
  match tracks.items with
  | [] -> return None
  | track :: _ -> return (Some track)
