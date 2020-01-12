open Core
open Async

val parse_response_body
  :  Cohttp.Response.t
  -> string
  -> of_yojson:(Yojson.Safe.t -> ('a, string) Result.t)
  -> 'a Or_error.t

val iter_until_error
  :  'a Pipe.Reader.t
  -> f:('a -> unit Or_error.t Deferred.t)
  -> unit Or_error.t Deferred.t
