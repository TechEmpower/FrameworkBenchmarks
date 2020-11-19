let listen ~request_handler ~error_handler port =
  let open Lwt.Infix in
  let listen_address = Unix.(ADDR_INET (inet_addr_any, port)) in
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket listen_address
        (Piaf.Server.create ?config:None ~error_handler request_handler)
      >|= fun _server ->
      Printf.printf "Listening on port %i and echoing POST requests.\n%!" port);
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
