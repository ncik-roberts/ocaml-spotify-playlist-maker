open Core
open Async

val listen_for_authorization_code
  :  client_id:string
  -> port:int
  -> scopes:Spotify.Scope.t list
  -> Uri.t * Spotify.Authorization_code_flow.Authorization_code.t Or_error.t Deferred.t
