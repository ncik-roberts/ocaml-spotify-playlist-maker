open Core
open Async

module Request_error : sig
  type t =
    { response_code : int
    ; error : Error.t
    }
end

module Debug_mode : sig
  type t =
    { print_requests : bool
    ; print_responses : bool
    }
end

type t

val create : debug_mode:Debug_mode.t option -> t
val request : t -> 'a Cohttp_request.t -> ('a, Request_error.t) Result.t Deferred.t
