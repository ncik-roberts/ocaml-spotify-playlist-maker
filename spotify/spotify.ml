open Core
open Async

module Credentials = struct
  type t =
    { client_id : string
    ; client_secret : string
    }
  [@@deriving fields]

  let create = Fields.create
end

let debug = ref false
let set_debug_mode = (:=) debug

module Access_token = struct
  type _ t = string
end

let random_state = Random.State.make_self_init ()

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

module type Jsonable = sig
  type t [@@deriving yojson]
end

module Ignore_json_result : Jsonable with type t = unit = struct
  type t = unit [@@deriving yojson]

  let of_yojson _ = Ok ()
end

module Get_access_token = struct
  let get_access_token
      ~body
      ~credentials:{ Credentials.client_id; client_secret }
      ~of_yojson
    =
    let uri = Uri.of_string "https://accounts.spotify.com/api/token" in
    match Base64.encode (sprintf "%s:%s" client_id client_secret) with
    | Error (`Msg error) -> return (Error (Error.of_string error))
    | Ok base64_encoded ->
      let headers =
        Cohttp.Header.of_list
          [ "Authorization", sprintf "Basic %s" base64_encoded
          ; "Content-Type", "application/x-www-form-urlencoded"
          ]
      in
      let body = String.concat body ~sep:"&" |> Cohttp_async.Body.of_string in
      let%bind response, response_body =
        Cohttp_async.Client.post ~body ~headers uri
      in
      if !debug then print_endline "Requesting access token...";
      let%bind response_body = Cohttp_async.Body.to_string response_body in
      return (Cohttp_async_utils.parse_response_body response response_body ~of_yojson)
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
    Get_access_token.get_access_token ~of_yojson ~body:[ "grant_type=client_credentials" ]
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

  let generate_client_uri_for_authorization_code
      ~client_id
      ~redirect_uri
      ~scopes
    =
    let secret_state = Random.State.bits random_state |> sprintf "%x" in
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

  let get_access_token { Authorization_code.redirect_uri; code } =
    Get_access_token.get_access_token
      ~of_yojson
      ~body:
        [ "grant_type=authorization_code"
        ; sprintf !"redirect_uri=%{Uri}" redirect_uri
        ; sprintf "code=%s" code
        ]

  let refresh_access_token refresh_token =
    Get_access_token.get_access_token
      ~of_yojson
      ~body:
        [ "grant_type=refresh_token"
        ; sprintf "refresh_token=%s" refresh_token
        ]
end

module Query = struct
  type t = string [@@deriving sexp, compare]

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
    [@@deriving sexp_of, yojson { strict = false }]
end

module Paging_object = struct
  type 'a t = { items : 'a list; total : int }
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

  let of_id id = { id }
end

let request
    (type request response)
    ~(request : [ `Get | `Post of request * (module Jsonable with type t = request) ])
    ~response:(module Response : Jsonable with type t = response)
    ~access_token
    ~uri
    : response Or_error.t Deferred.t
  =
  let content_type_header =
    match request with
    | `Get -> None
    | `Post _ -> Some ("Content-Type", "application/json")
  in
  let headers =
    [ Some ("Authorization", sprintf "Bearer %s" access_token)
    ; content_type_header
    ]
    |> List.filter_map ~f:Fn.id
    |> Cohttp.Header.of_list
  in
  let%bind response, body =
    match request with
    | `Get ->
      if !debug then printf !"Getting %{Uri}\n" uri;
      Cohttp_async.Client.get uri ~headers
    | `Post (body, (module J)) ->
      let body = J.to_yojson body |> Yojson.Safe.to_string in
      if !debug then printf !"Posting %{Uri}\n" uri;
      Cohttp_async.Client.post uri ~headers ~body:(Cohttp_async.Body.of_string body)
  in
  let%bind body = Cohttp_async.Body.to_string body in
  Cohttp_async_utils.parse_response_body response body ~of_yojson:Response.of_yojson
  |> return

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
  request
    ~access_token
    ~uri
    ~request:(`Post ({ Playlist_body.name; public }, (module Playlist_body)))
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
    ~request:(`Post (body, (module Add_to_playlist_body)))
    ~response:(module Ignore_json_result)

let search_tracks ~access_token ~query =
  let uri =
    let base_uri = Uri.of_string "https://api.spotify.com/v1/search" in
    Uri.add_query_params base_uri
      [ "q", [ query ]
      ; "type", [ "track" ]
      ]
  in
  request
    ~access_token
    ~uri
    ~request:`Get
    ~response:(module Tracks)
  >>| Result.map ~f:(fun x -> x.Tracks.tracks.items)

module Playlist_item = struct
  type t = { track : Track.t } [@@deriving yojson { strict = false }]
end

let lookup_playlist ~access_token ~(playlist : Playlist.t) =
  let uri =
    sprintf "https://api.spotify.com/v1/playlists/%s/tracks" playlist.id
    |> Uri.of_string
  in
  let open Deferred.Or_error.Let_syntax in
  let get_tracks ~offset =
    let uri = Uri.add_query_param uri ("offset", [ sprintf "%d" offset ]) in
    request
      ~access_token
      ~uri
      ~request:`Get
      ~response:(module struct
        type t = Playlist_item.t Paging_object.t [@@deriving yojson { strict = false }]
      end)
    >>| Paging_object.map ~f:(fun { Playlist_item.track } -> track)
  in
  let%bind tracks = get_tracks ~offset:0 in
  if tracks.total <= 100 then return tracks.items
  else
    let rec loop offset ~state =
      if offset >= tracks.total then return (List.rev state)
      else
        let%bind tracks = get_tracks ~offset in
        loop (offset + 100) ~state:(List.rev tracks.items @ state)
    in
    loop 100 ~state:(List.rev tracks.items)

