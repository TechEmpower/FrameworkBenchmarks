val plaintext: unit -> string

val json: unit -> Yojson.Safe.t

val single_query: unit -> Yojson.Safe.t Lwt.t

val multiple_queries: int -> Yojson.Safe.t Lwt.t

val updates: int -> Yojson.Safe.t Lwt.t

val fortunes: unit -> [> Html_types.html ] Tyxml_html.elt Lwt.t
