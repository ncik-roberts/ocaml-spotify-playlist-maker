open Types

module Jsonable = struct
  open Jsonable
  module Of = Of
  module To = To

  module type S = S

  module Ignore : Of.S with type t = unit = struct
    type t = unit

    let of_yojson _ = Ok ()
  end
end

module Body = Body
module Request_type = Request_type
include Request
