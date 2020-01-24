open Core
open Async

type t

module Config : sig
  type t =
    { port : int
          (** A free port on localhost. This port will be used by this client
       *  to listen for the client authorization flow token.
       *)
    ; credentials : Spotify.Credentials.t
    ; user_id : string
    ; debug : bool
    }
end

module Param : sig
  val param : (scopes:Spotify.Scope.t list -> Uri.t * t Deferred.t) Command.Param.t
  val from_config_file : t Deferred.t Command.Param.t

  val from_config_file_refreshing_refresh_token
    : (scopes:Spotify.Scope.t list -> Uri.t * t Deferred.t) Command.Param.t
end

(* A human must visit the returned URI to authorize the Spotify app.
 * Only then will the returned deferred become determined.
 *)
val create
  :  ?write_config_to:Filename.t
  -> Config.t
  -> scopes:Spotify.Scope.t list
  -> Uri.t * t Or_error.t Deferred.t

val config : t -> Config.t

val create_with_refresh_token
  :  Config.t
  -> Spotify.Authorization_code_flow.Refresh_token.t
  -> t Or_error.t Deferred.t

val make_playlist
  :  t
  -> kind:[ `Private | `Public ]
  -> name:string
  -> Spotify.Playlist.t Or_error.t Deferred.t

val all_tracks_in_playlist
  :  t
  -> playlist:Spotify.Playlist.t
  -> Spotify.Track.t list Or_error.t Deferred.t

val add_to_playlist
  :  t
  -> playlist:Spotify.Playlist.t
  -> tracks:Spotify.Track.t list
  -> unit Or_error.t Deferred.t

val search_track
  :  t
  -> query:Spotify.Query.t
  -> Spotify.Track.t option Or_error.t Deferred.t
