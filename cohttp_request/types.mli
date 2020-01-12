module Jsonable : sig
  module To : sig
    module type S = sig
      type t [@@deriving to_yojson]
    end
  end

  module Of : sig
    module type S = sig
      type t [@@deriving of_yojson]
    end
  end

  module type S = sig
    type t [@@deriving yojson]
  end
end

module Body : sig
  type t =
    | Url_encoded : (string * string) list -> t
    | Json_encoded :
        { body : 'body
        ; body_to_yojson : (module Jsonable.To.S with type t = 'body)
        }
        -> t
end

module Request_type : sig
  type t =
    | Get
    | Post of Body.t
  [@@deriving sexp_of]
end

module Request : sig
  type 'response t =
    { uri : Uri.t
    ; headers : Cohttp.Header.t
    ; request_type : Request_type.t
    ; response_of_yojson : (module Jsonable.Of.S with type t = 'response)
    }

  val map : f:('a -> 'b) -> 'a t -> 'b t
end
