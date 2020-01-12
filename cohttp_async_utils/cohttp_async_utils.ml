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

let iter_until_error reader ~f =
  Pipe.fold reader ~init:(Ok ()) ~f:(fun ok x ->
    let () =
      match ok with
      | Ok () -> ()
      | Error e -> raise_s [%message "Unexpected error" (e : Error.t)]
    in
    match%bind f x with
    | Ok () -> return (Ok ())
    | Error e ->
      Pipe.close_read reader;
      let%bind () = Pipe.closed reader in
      return (Error e))
