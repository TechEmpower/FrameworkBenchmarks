let make (handler: Morph.Server.handler) request =
  let open Lwt.Infix in
  handler(request) >|= Morph.Response.add_headers [
    ("Date", (Ptime.to_rfc3339 (Ptime_clock.now ())));
    ("Server", "Morph");
  ] 
