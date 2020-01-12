open Core
open Async

let parse_response_body response response_body ~of_yojson =
  match Cohttp.Response.status response with
  | #Cohttp.Code.success_status ->
    (match Or_error.try_with (fun () -> Yojson.Safe.from_string response_body) with
     | Error error -> Error error
     | Ok yojson -> of_yojson yojson |> Result.map_error ~f:Error.of_string)
  | code ->
    error_s
      [%message "unexpected response code"
        (code : Cohttp.Code.status_code)
        (response : Cohttp.Response.t)
        (response_body : string)
      ]

let request
  (type response)
  ({ headers
   ; uri
   ; request_type
   ; response_of_yojson =
       (module Response : Cohttp_request.Jsonable.Of.S with type t = response)
   } : response Cohttp_request.t) =
  let%bind response, response_body =
    match request_type with
    | Get -> Cohttp_async.Client.get ~headers uri
    | Post (Url_encoded body) ->
      let body =
        List.map body ~f:(fun (k, v) -> (k, [v]))
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
        Body.to_yojson body
        |> Yojson.Safe.to_string
        |> Cohttp_async.Body.of_string
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
  return (parse_response_body response response_body ~of_yojson:Response.of_yojson)

