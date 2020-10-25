module World = struct
  type t = { id: int; randomNumber: int }[@@deriving yojson]
  type list_response = (t list)[@@deriving yojson]
  type message_response = { message: string }[@@deriving yojson]
end

module Fortune = struct
  type t = { id: int; message: string }[@@deriving yojson]
  let compare a b = String.compare a.message b.message
end
