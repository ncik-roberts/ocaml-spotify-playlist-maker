open Core

module Credentials = struct
  type t =
    { client_id : string
    ; client_secret : string
    }
  [@@deriving fields]

  let create = Fields.create
end

module Access_token = struct
  type _ t = string
end

module Scope = struct
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

  let to_uri = function
    | `App_remote_control -> "app-remote-control"
    | `Playlist_modify_private -> "playlist-modify-private"
    | `Playlist_modify_public -> "playlist-modify-public"
    | `Playlist_read_collaborative -> "playlist-read-collaborative"
    | `Playlist_read_private -> "playlist-read-private"
    | `Streaming -> "streaming"
    | `Ugc_image_upload -> "ugc_image_upload"
    | `User_follow_modify -> "user-follow-modify"
    | `User_follow_read -> "user-follow-read"
    | `User_library_modify -> "user-library-modify"
    | `User_library_read -> "user-library-read"
    | `User_modify_playback_state -> "user-modify-playback-state"
    | `User_read_currently_playing -> "user-read-currently-playing"
    | `User_read_email -> "user-read-email"
    | `User_read_playback_state -> "user-read-playback-state"
    | `User_read_private -> "user-read-private"
    | `User_read_recently_played -> "user-read-recently-played"
    | `User_top_read -> "user-top-read"
end

module Get_access_token = struct
  let get_access_token
      (type response)
      ~body
      ~credentials:{ Credentials.client_id; client_secret }
      ~(response_of_yojson : Yojson.Safe.t -> (response, string) Result.t)
    =
    let uri = Uri.of_string "https://accounts.spotify.com/api/token" in
    match Base64.encode (sprintf "%s:%s" client_id client_secret) with
    | Error (`Msg error) ->
      failwithf
        "Base64 encoding of the credentials failed -- bug in Base64 library?\
        \ Original error: %s"
        error
        ()
    | Ok base64_encoded ->
      { Cohttp_request.uri
      ; headers =
          Cohttp.Header.of_list
            [ "Authorization", sprintf "Basic %s" base64_encoded
            ; "Content-Type", "application/x-www-form-urlencoded"
            ]
      ; request_type = Post (Url_encoded body)
      ; response_of_yojson =
          (module struct
            type t = response [@@deriving of_yojson]
          end)
      }
end

module Client_credentials_flow = struct
  module Access_token = struct
    type t = string [@@deriving yojson]
  end

  type t =
    { access_token : Access_token.t
    ; expires_in : int
    }
    [@@deriving yojson { strict = false }]

  let get_access_token =
    Get_access_token.get_access_token
      ~response_of_yojson:of_yojson
      ~body:[ "grant_type", "client_credentials" ]
end

module Authorization_code_flow = struct
  module Authorization_code = struct
    type t =
      { redirect_uri : Uri.t
      ; code : string
      }

    let of_code code ~redirect_uri =
      { redirect_uri
      ; code
      }
  end

  module Access_token = struct
    type t = string [@@deriving yojson]
  end

  module Refresh_token = struct
    type t = string [@@deriving yojson]
  end

  type t =
    { access_token : Access_token.t
    ; expires_in : int
    ; refresh_token : Refresh_token.t
    }
    [@@deriving yojson { strict = false }]

  let secret_state = lazy (Random.State.make_self_init ())

  let generate_client_uri_for_authorization_code
      ~client_id
      ~redirect_uri
      ~scopes
    =
    let secret_state = Random.State.bits (force secret_state) |> sprintf "%x" in
    let base_uri = Uri.of_string "https://accounts.spotify.com/authorize" in
    let uri = Uri.add_query_params base_uri
      [ "client_id", [ client_id ]
      ; "response_type", [ "code" ]
      ; "redirect_uri", [ Uri.to_string redirect_uri ]
      ; "state", [ secret_state ]
      ; "scope", [ List.map ~f:Scope.to_uri scopes |> String.concat ~sep:" " ]
      ]
    in
    uri, `Secret_state secret_state

  let get_access_token { Authorization_code.redirect_uri; code } ~credentials =
    Get_access_token.get_access_token
      ~credentials
      ~response_of_yojson:of_yojson
      ~body:
        [ "grant_type", "authorization_code"
        ; "redirect_uri", Uri.to_string redirect_uri
        ; "code", code
        ]

  let refresh_access_token refresh_token ~credentials =
    let
      module Access_token_without_refresh_token = struct
        type t =
          { access_token : Access_token.t
          ; expires_in : int
          }
          [@@deriving yojson { strict = false }]
      end
    in
    Get_access_token.get_access_token
      ~credentials
      ~response_of_yojson:Access_token_without_refresh_token.of_yojson
      ~body:
        [ "grant_type", "refresh_token"
        ; "refresh_token", refresh_token
        ]
    |> Cohttp_request.map ~f:(fun { Access_token_without_refresh_token.access_token; expires_in } ->
        { access_token; expires_in; refresh_token })
end

module Query = struct
  type t = string [@@deriving compare]

  let create ~album ~artist ~track =
    [ "album", album
    ; "artist", artist
    ; "track", track
    ]
    |> List.filter_map ~f:(fun (tag, value) ->
        let%map.Option.Let_syntax value = value in
        sprintf "%s:%s" tag value)
    |> String.concat ~sep:" "
end

module Track = struct
  type t =
    { uri : string
    ; name : string
    }
    [@@deriving yojson { strict = false }]
end

module Paging_object = struct
  type 'a t =
    { items : 'a list
    ; limit : int
    ; offset : int
    ; total : int
    }
    [@@deriving yojson { strict = false }]

  let map ~f t = { t with items = List.map ~f t.items }
end

module Tracks = struct
  type t = { tracks : Track.t Paging_object.t }
    [@@deriving yojson { strict = false }]
end

module Playlist = struct
  type t = { id : string }
    [@@deriving yojson { strict = false }]

  let parse = function
    | `Id id -> Ok { id }
    | `Uri uri ->
        try Scanf.sscanf uri "spotify:track:%s" (fun id -> Ok { id })
        with _ -> Error `Invalid_playlist

end

let paging_query_params ~offset ~limit =
  [ Option.map offset ~f:(fun offset -> ("offset", [ sprintf "%d" offset ]))
  ; Option.map limit ~f:(fun limit -> "limit", [ sprintf "%d" limit ])
  ]
  |> List.filter_map ~f:Fn.id

let request ~access_token ~uri ~request ~response =
  { Cohttp_request.uri
  ; headers =
      Cohttp.Header.of_list [ "Authorization", sprintf "Bearer %s" access_token ]
  ; request_type = request
  ; response_of_yojson = response
  }

let make_playlist ~kind ~access_token ~user_id ~name =
  let uri =
    Uri.of_string (sprintf "https://api.spotify.com/v1/users/%s/playlists" user_id)
  in
  let
    module Playlist_body = struct
      type t = { name : string; public : bool } [@@deriving yojson]
    end
  in
  let public =
    match kind with
    | `Public -> true
    | `Private -> false
  in
  let body = { Playlist_body.name; public } in
  request
    ~uri
    ~access_token
    ~request:(Post (Json_encoded { body; body_to_yojson = (module Playlist_body) }))
    ~response:(module Playlist)

let add_to_playlist ~access_token ~(playlist : Playlist.t) ~(tracks : Track.t list) =
  let uri =
    Uri.of_string (sprintf "https://api.spotify.com/v1/playlists/%s/tracks" playlist.id)
  in
  let
    module Add_to_playlist_body = struct
      type t = { uris : string list } [@@deriving yojson]
    end
  in
  let body =
    { Add_to_playlist_body.uris = List.map tracks ~f:(fun t -> t.uri) }
  in
  request
    ~access_token
    ~uri
    ~request:(Post (Json_encoded { body; body_to_yojson = (module Add_to_playlist_body) }))
    ~response:(module Cohttp_request.Jsonable.Ignore)

let search_tracks ?offset ?limit () ~access_token ~query =
  let uri =
    let base_uri = Uri.of_string "https://api.spotify.com/v1/search" in
    Uri.add_query_params base_uri
      ([ "q", [ query ]
       ; "type", [ "track" ]
       ]
       @ paging_query_params ~offset ~limit)
  in
  request
    ~access_token
    ~uri
    ~request:Get
    ~response:(module Tracks)
  |> Cohttp_request.map ~f:(fun x -> x.Tracks.tracks)

module Playlist_item = struct
  type t = { track : Track.t } [@@deriving yojson { strict = false }]
end

let lookup_playlist ?offset ?limit () ~access_token ~(playlist : Playlist.t) =
  let uri =
    sprintf "https://api.spotify.com/v1/playlists/%s/tracks" playlist.id
    |> Uri.of_string
  in
  let query_params = paging_query_params ~offset ~limit in
  let uri = Uri.add_query_params uri query_params in
  request
    ~access_token
    ~uri
    ~request:Get
    ~response:(module struct
       type t = Playlist_item.t Paging_object.t [@@deriving yojson]
     end)
  |> Cohttp_request.map ~f:(Paging_object.map ~f:(fun item -> item.Playlist_item.track))
