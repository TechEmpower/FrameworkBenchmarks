module S = Tiny_httpd

let () =
  let server = S.create ~addr:"0.0.0.0" ~port:8080 () in
  let headers = [ ("Server", "tiny_httpd") ] in
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
  let nproc =
    match Sys.getenv "CORE_COUNT" with
    | x -> int_of_string x
    | exception Not_found ->
        Unix.open_process_in "getconf _NPROCESSORS_ONLN"
        |> input_line
        |> int_of_string
  in
  for i = 1 to nproc do
    flush_all ();
    if Unix.fork () = 0 then (
      (* child *)
      Printf.eprintf "Listening on %s:%d (child %d)\n" (S.addr server)
        (S.port server) i;
      match S.run server with
      | Ok () -> ()
      | Error e -> raise e )
  done;
  while true do
    Unix.pause ()
  done
