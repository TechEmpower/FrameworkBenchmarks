module S = Tiny_httpd

let () =
  let server = S.create ~addr:"0.0.0.0" ~max_connections:256 () in
  let headers = [ ("Server", "tiny_httpd") ] in
  (* say hello *)
  S.add_route_handler ~meth:`GET server
    S.Route.(exact "plaintext" @/ string @/ return)
    (fun _ _req ->
      let headers = S.Headers.set "Content-Type" "text/plain" headers in
      let headers = S.Headers.set "Date" (Lib.Time.now ()) headers in
      S.Response.make_string ~headers (Ok "Hello, World!"));
  S.add_route_handler ~meth:`GET server
    S.Route.(exact "json" @/ string @/ return)
    (fun _ _req ->
      let headers = S.Headers.set "Content-Type" "application/json" headers in
      let headers = S.Headers.set "Date" (Lib.Time.now ()) headers in
      let json = Lib.Message_t.{ message = "Hello, World!" } in
      S.Response.make_string ~headers
        (Ok (Lib.Message_j.string_of_message json)));
  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
