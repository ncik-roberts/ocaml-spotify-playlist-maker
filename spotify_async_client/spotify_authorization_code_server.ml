open! Core
open! Async

let listen_for_authorization_code ~client_id ~port ~scopes =
  let redirect_uri = sprintf "http://localhost:%d" port |> Uri.of_string in
  let client_uri, `Secret_state secret_state =
    Spotify.Authorization_code_flow.generate_client_uri_for_authorization_code
      ~client_id
      ~scopes
      ~redirect_uri
  in
  let deferred =
    let result = Ivar.create () in
    let%bind server =
      Cohttp_async.Server.create
        ~on_handler_error:`Raise
        (Tcp.Where_to_listen.of_port port)
        (fun ~body:_ _ request ->
          let request_uri = Cohttp_async.Request.uri request in
          let code_and_error =
            let open Option.Let_syntax in
            let get = Uri.get_query_param request_uri in
            let%bind state_query_param = get "state" in
            let%bind () =
              if String.equal state_query_param secret_state then Some () else None
            in
            let code = get "code" in
            let error = get "error" in
            return (code, error)
          in
          let () =
            match code_and_error with
            | None | Some (None, None) -> ()
            | Some (Some code, _) ->
              let authorization_flow =
                Spotify.Authorization_code_flow.Authorization_code.of_code
                  ~redirect_uri
                  code
              in
              Ivar.fill_if_empty result (Ok authorization_flow)
            | Some (_, Some error) ->
              Ivar.fill_if_empty result (Error (Error.of_string error))
          in
          return (Cohttp_async.Response.make (), Cohttp_async.Body.empty))
    in
    let%bind result = Ivar.read result in
    let%bind () = Cohttp_async.Server.close server in
    return result
  in
  client_uri, deferred
;;
