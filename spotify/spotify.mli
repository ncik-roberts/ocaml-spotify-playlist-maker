open Core
open Async

val set_debug_mode : bool -> unit

module Credentials : sig
  type t

  val create : client_id:string -> client_secret:string -> t
end

module Access_token : sig
  type _ t
end

module Client_credentials_flow : sig
  module Access_token : sig
    type t = [ `client_credentials_flow ] Access_token.t
  end

  type t =
    { access_token : Access_token.t
    ; expires_in : int
    }

  val get_access_token : credentials:Credentials.t -> t Or_error.t Deferred.t
end

module Scope : sig
  type t =
    [ `App_remote_control
    | `Playlist_modify_private
    | `Playlist_modify_public
    | `Playlist_read_collaborative
    | `Playlist_read_private
    | `Streaming
    | `Ugc_image_upload
    | `User_follow_modify
    | `User_follow_read
    | `User_library_modify
    | `User_library_read
    | `User_modify_playback_state
    | `User_read_currently_playing
    | `User_read_email
    | `User_read_playback_state
    | `User_read_private
    | `User_read_recently_played
    | `User_top_read
    ]
end

module Authorization_code_flow : sig
  module Authorization_code : sig
    type t

    val of_code : string -> redirect_uri:Uri.t -> t
  end

  module Access_token : sig
    type t = [ `authorization_code_flow ] Access_token.t
  end

  module Refresh_token : sig
    type t
  end

  type t =
    { access_token : Access_token.t
    ; expires_in : int
    ; refresh_token : Refresh_token.t
    }

  val generate_client_uri_for_authorization_code
    :  client_id:string
    -> redirect_uri:Uri.t
    -> scopes:Scope.t list
    -> Uri.t * [ `Secret_state of string ]

  val get_access_token
    :  Authorization_code.t
    -> credentials:Credentials.t
    -> t Or_error.t Deferred.t

  val refresh_access_token
    :  Refresh_token.t
    -> credentials:Credentials.t
    -> t Or_error.t Deferred.t
end

module Query : sig
  type t [@@deriving sexp_of, compare]

  val create
    :  album:string option
    -> artist:string option
    -> track:string option
    -> t
end

module Track : sig
  type t =
    { uri : string
    ; name : string
    }
    [@@deriving sexp_of]
end

module Playlist : sig
  type t
  val of_id : string -> t
end

val make_playlist
  :  kind:[ `Private | `Public ]
  -> access_token:Authorization_code_flow.Access_token.t
  -> user_id:string
  -> name:string
  -> Playlist.t Or_error.t Deferred.t

val add_to_playlist
  :  access_token:Authorization_code_flow.Access_token.t
  -> playlist:Playlist.t
  -> tracks:Track.t list
  -> unit Or_error.t Deferred.t

val search_tracks
  :  access_token:_ Access_token.t
  -> query:Query.t
  -> Track.t list Or_error.t Deferred.t

val lookup_playlist
  :  access_token:_ Access_token.t
  -> playlist:Playlist.t
  -> Track.t list Or_error.t Deferred.t
