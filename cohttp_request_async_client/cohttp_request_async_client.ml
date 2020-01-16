open Core
open Async

module Request_error = struct
  type t =
    { response_code : int
    ; error : Error.t
    }
end

module Debug_mode = struct
  type t =
    { print_requests : bool
    ; print_responses : bool
    }
end

type t = { debug_mode : Debug_mode.t }

let create ~debug_mode =
  Option.value_map
    debug_mode
    ~f:(fun debug_mode -> { debug_mode })
    ~default:{ debug_mode = { print_requests = false; print_responses = false } }
;;

let parse_response_body t response response_body ~of_yojson =
  if t.debug_mode.print_responses then print_endline response_body;
  let status = Cohttp.Response.status response in
  let result =
    match status with
    | #Cohttp.Code.success_status ->
      (match Or_error.try_with (fun () -> Yojson.Safe.from_string response_body) with
      | Error error -> error_s [%message "JSON parsing failed" (error : Error.t)]
      | Ok yojson -> of_yojson yojson |> Result.map_error ~f:Error.of_string)
    | code ->
      error_s
        [%message
          "unexpected response code"
            (code : Cohttp.Code.status_code)
            (response : Cohttp.Response.t)
            (response_body : string)]
  in
  match result with
  | Ok result -> Ok result
  | Error error ->
    Error { Request_error.response_code = Cohttp.Code.code_of_status status; error }
;;

let request
    (type response)
    t
    ({ headers
     ; uri
     ; request_type
     ; response_of_yojson =
         (module Response : Cohttp_request.Jsonable.Of.S with type t = response)
     } :
      response Cohttp_request.t)
  =
  if t.debug_mode.print_requests then print_endline (Uri.to_string uri);
  let%bind response, response_body =
    match request_type with
    | Get -> Cohttp_async.Client.get ~headers uri
    | Post (Url_encoded body) ->
      let body =
        List.map body ~f:(fun (k, v) -> k, [ v ])
        |> Uri.encoded_of_query
        |> Cohttp_async.Body.of_string
      in
      let headers =
        Cohttp.Header.add_unless_exists
          headers
          "content-type"
          "application/x-www-form-urlencoded"
      in
      Cohttp_async.Client.post ~headers ~body uri
    | Post (Json_encoded { body; body_to_yojson = (module Body) }) ->
      let body =
        Body.to_yojson body |> Yojson.Safe.to_string |> Cohttp_async.Body.of_string
      in
      let headers =
        Cohttp.Header.add_unless_exists
          headers
          "content-type"
          "application/x-www-form-urlencoded"
      in
      Cohttp_async.Client.post ~headers ~body uri
  in
  let%bind response_body = Cohttp_async.Body.to_string response_body in
  parse_response_body t response response_body ~of_yojson:Response.of_yojson
  |> Result.map_error ~f:(fun { Request_error.response_code; error } ->
         let error =
           Error.create_s
             [%message
               "Got invalid response for request"
                 (error : Error.t)
                 ~uri:(Uri.to_string uri : string)
                 (headers : Cohttp.Header.t)
                 (request_type : Cohttp_request.Request_type.t)]
         in
         { Request_error.response_code; error })
  |> return
;;
