open Opium.Std
open Lwt.Syntax

let run_app app ~instances ~port =
  let listen_address =
    let inet_addr = Unix.inet_addr_any in
    Unix.ADDR_INET (inet_addr, port)
  in
  let socket =
    Lwt_unix.socket (Unix.domain_of_sockaddr listen_address) Unix.SOCK_STREAM 0
  in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;

  Lwt_main.run (
    let+ () = Lwt_unix.bind socket listen_address in
    Lwt_unix.listen socket (Lwt_unix.somaxconn () [@ocaml.warning "-3"])
  );

  let rec accept_loop socket handler instance =
    let* (socket', sockaddr') = Lwt_unix.accept socket in
    Lwt.async (fun () -> handler sockaddr' socket');
    accept_loop socket handler instance
  in

  let rock = App.to_rock app in

  for i = 1 to instances do
    flush_all ();
    if Lwt_unix.fork () = 0 then (
      (* child *)
      Server.start_refreshing_date ();
      Lwt.async (fun () ->
          let* () = Lwt_io.eprintf "Listening on %d (child %d)\n" port i in
          let handler = Server.create_connection_handler rock in
          accept_loop socket handler i
        );
      let forever, _ = Lwt.wait () in
      Lwt_main.run forever;
      exit 0)
  done;

  while true do
    Unix.pause ()
  done


let main () =
  let port =
    match Sys.getenv_opt "PORT" with
    | Some x -> int_of_string x
    | None -> 8080
  in
  let instances =
    match Sys.getenv_opt "APP_INSTANCES" with
    | Some x -> int_of_string x
    | None ->
      let ic = Unix.open_process_in "getconf _NPROCESSORS_ONLN" in
      let cores = int_of_string (input_line ic) in
      ignore (Unix.close_process_in ic);
      cores
  in

  Server.dump_lwt ();
  Printf.eprintf "Starting %d instances\n" instances;

  let app = Server.create_app ~port in
  run_app app ~instances ~port

let () = main ()
