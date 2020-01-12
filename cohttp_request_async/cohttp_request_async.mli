open Core
open Async

val request : 'a Cohttp_request.t -> 'a Or_error.t Deferred.t
