let client_address_key : Unix.sockaddr Hmap.key = Hmap.Key.create ()

let wrap_context (next : Morph.Server.handler)
    (req : Unix.sockaddr Piaf.Server.ctx) =
  let open Lwt.Infix in
  let ctx = Hmap.add client_address_key req.ctx Hmap.empty in
  next { req with ctx } >|= Morph.Response.response_of_result
