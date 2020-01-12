open Core
open Async

val fetch_authorization_code
  :  client_id:string
  -> port:int
  -> scopes:Spotify.Scope.t list
  -> Spotify.Authorization_code_flow.Authorization_code.t Or_error.t Deferred.t
