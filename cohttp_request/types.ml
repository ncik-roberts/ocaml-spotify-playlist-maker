open Core

module Jsonable = struct
  module To = struct
    module type S = sig
      type t [@@deriving to_yojson]
    end
  end

  module Of = struct
    module type S = sig
      type t [@@deriving of_yojson]
    end
  end

  module type S = sig
    type t [@@deriving yojson]
  end
end

module Body = struct
  type t =
    | Url_encoded : (string * string) list -> t
    | Json_encoded :
        { body : 'body
        ; body_to_yojson : (module Jsonable.To.S with type t = 'body)
        }
        -> t

  let sexp_of_t = function
    | Url_encoded body -> [%message "Url_encoded" (body : (string * string) list)]
    | Json_encoded { body; body_to_yojson = (module Body) } ->
      let yojson = Body.to_yojson body in
      [%message "Json_encoded" ~body:(Yojson.Safe.to_string yojson : string)]
end

module Request_type = struct
  type t =
    | Get
    | Post of Body.t
  [@@deriving sexp_of]
end

module Request = struct
  type 'response t =
    { uri : Uri.t
    ; headers : Cohttp.Header.t
    ; request_type : Request_type.t
    ; response_of_yojson : (module Jsonable.Of.S with type t = 'response)
    }

  let map (type a b) ~(f : a -> b) (t : a t) : b t =
    let module From = (val t.response_of_yojson) in
    { t with response_of_yojson =
        (module struct
           type t = b
           let of_yojson yojson = Result.map ~f (From.of_yojson yojson)
         end)
    }
end
