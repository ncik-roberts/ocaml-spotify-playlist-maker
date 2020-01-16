open Core
open Async

type t

module Song : sig
  type t =
    { title : string
    ; artist : string
    ; album : string option
    ; start_time : Time_ns.t
    }
  [@@deriving sexp_of]
end

val create : debug_mode:bool -> station_id:string -> t

val lookup_songs
  :  t
  -> from:Time_ns.t
  -> until:Time_ns.t
  -> Song.t Or_error.t Pipe.Reader.t
