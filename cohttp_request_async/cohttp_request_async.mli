open Core
open Async

module Request_error : sig
  type t =
    { response_code : int
    ; error : Error.t
    }
end

val request : 'a Cohttp_request.t -> ('a, Request_error.t) Result.t Deferred.t
