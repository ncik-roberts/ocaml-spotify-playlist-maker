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
    ; scopes : Spotify.Scope.t list
    ; user_id : string
    }
end

(* A human must visit the returned URI to authorize the Spotify app.
 * Only then will the returned deferred become determined.
 *)
val create : Config.t -> Uri.t * t Or_error.t Deferred.t

val make_playlist
  :  t
  -> kind:[ `Private | `Public ]
  -> name:string
  -> Spotify.Playlist.t Or_error.t Deferred.t

val all_tracks_in_playlist :
  t -> playlist:Spotify.Playlist.t -> Spotify.Track.t list Or_error.t Deferred.t

val add_to_playlist :
  t -> playlist:Spotify.Playlist.t -> tracks:Spotify.Track.t list -> unit Or_error.t Deferred.t

val search_track : t -> query:Spotify.Query.t -> Spotify.Track.t option Or_error.t Deferred.t
