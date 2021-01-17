(* This code is borrowed from this PR: https://github.com/anmonteiro/piaf/pull/80 *)
open Lwt.Infix

type server = { shutdown: unit Lwt.t Lazy.t  }

let dump_lwt () =
  let options =
    [ "fd_passing", `fd_passing
    ; "fdatasync", `fdatasync
    ; "get_affinity", `get_affinity
    ; "get_cpu", `get_cpu
    ; "get_credentials", `get_credentials
    ; "libev", `libev
    ; "madvise", `madvise
    ; "mincore", `mincore
    ; "recv_msg", `recv_msg
    ; "send_msg", `send_msg
    ; "set_affinity", `set_affinity
    ; "wait4", `wait4
    ]
  in
  Format.eprintf "Lwt:\n%a@."
    (Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
      (fun fmt (str, opt) -> Format.fprintf fmt "  %s = %b" str (Lwt_sys.have opt)))
    options

let close_socket fd =
  Lwt.finalize
    (fun () ->
       Lwt.catch
         (fun () ->
            Lwt_unix.shutdown fd Unix.SHUTDOWN_ALL;
            Lwt.return_unit)
         (function
           (* Occurs if the peer closes the connection first. *)
           | Unix.Unix_error (Unix.ENOTCONN, _, _) -> Lwt.return_unit
           | exn -> Lwt.fail exn))
    (fun () ->
       Lwt_unix.close fd)

let establish_server_generic
    ?fd:preexisting_socket_for_listening
    listening_address
    connection_handler_callback =

  let listening_socket =
    match preexisting_socket_for_listening with
    | None ->
      Lwt_unix.socket
        (Unix.domain_of_sockaddr listening_address) Unix.SOCK_STREAM 0
    | Some socket ->
      socket
  in
  Lwt_unix.setsockopt listening_socket Unix.SO_REUSEADDR true;

  (* This promise gets resolved with `Should_stop when the user calls
     Lwt_io.shutdown_server. This begins the shutdown procedure. *)
  let should_stop, notify_should_stop =
    Lwt.wait () in

  (* Some time after Lwt_io.shutdown_server is called, this function
     establish_server_generic will actually close the listening socket. At that
     point, this promise is resolved. This ends the shutdown procedure. *)
  let wait_until_listening_socket_closed, notify_listening_socket_closed =
    Lwt.wait () in

  let rec accept_loop () =
    let try_to_accept =
      Lwt_unix.accept listening_socket >|= fun x ->
      `Accepted x
    in

    Lwt.pick [try_to_accept; should_stop] >>= function
    | `Accepted (client_socket, client_address) ->
      begin
        try Lwt_unix.set_close_on_exec client_socket
        with Invalid_argument _ -> ()
      end;

      connection_handler_callback client_address client_socket;

      accept_loop ()

    | `Should_stop ->
      Lwt_unix.close listening_socket >>= fun () ->

      begin match listening_address with
      | Unix.ADDR_UNIX path when path <> "" && path.[0] <> '\x00' ->
        Unix.unlink path
      | _ ->
        ()
      end [@ocaml.warning "-4"];

      Lwt.wakeup_later notify_listening_socket_closed ();
      Lwt.return_unit
  in

  let server =
    {shutdown =
      lazy begin
        Lwt.wakeup_later notify_should_stop `Should_stop;
        wait_until_listening_socket_closed
      end}
  in

  (* Actually start the server. *)
  let server_has_started =
    (* bind_function listening_socket listening_address >>= fun () -> *)
    (* Lwt_unix.listen listening_socket backlog; *)

    Lwt.async accept_loop;

    Lwt.return_unit
  in

  server, server_has_started

let establish_server_with_client_socket
    ?server_fd ?(no_close = false) sockaddr f
  =
  let handler client_address client_socket =
    Lwt.async (fun () ->
        (* Not using Lwt.finalize here, to make sure that exceptions from [f]
           reach !Lwt.async_exception_hook before exceptions from closing the
           channels. *)
        Lwt.catch
          (fun () -> f client_address client_socket)
          (fun exn ->
            !Lwt.async_exception_hook exn;
            Lwt.return_unit)
        >>= fun () ->
        if no_close then
          Lwt.return_unit
        else if Lwt_unix.state client_socket = Lwt_unix.Closed then
          Lwt.return_unit
        else
          Lwt.catch
            (fun () -> close_socket client_socket)
            (fun exn ->
              !Lwt.async_exception_hook exn;
              Lwt.return_unit))
  in
  let server, server_started =
    establish_server_generic
      ?fd:server_fd
      sockaddr
      handler
  in
  server_started >>= fun () -> Lwt.return server


let listen ~request_handler ~error_handler port =
  let nproc =
    Unix.open_process_in "getconf _NPROCESSORS_ONLN"
    |> input_line
    |> int_of_string
  in
  Format.eprintf "Detected %d cores@." nproc;
  let ulimit_n =
    Unix.open_process_in "ulimit -n" |> input_line |> int_of_string
  in
  Format.eprintf "Detected %d max open files@." ulimit_n;
  dump_lwt ();
  let sockaddr = Unix.(ADDR_INET (inet_addr_any, port)) in
  let socket =
    Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
  in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  Lwt_main.run
  @@ ( Lwt_unix.bind socket sockaddr >|= fun () ->
  Lwt_unix.listen socket 10_000 );
  for i = 1 to nproc do
    flush_all ();
    let pid = Lwt_unix.fork () in
    if pid = 0 then (
      (* child *)
      (Lwt.async_exception_hook := fun exn -> raise exn);
      Lwt.async (fun () ->
          establish_server_with_client_socket
            ~server_fd:socket
            sockaddr
            (Piaf.Server.create ?config:None ~error_handler request_handler)
          >|= fun _server ->
          Format.eprintf "Listening on localhost:%i (child %d)@." port i);
      let forever, _ = Lwt.wait () in
      Lwt_main.run forever;
      exit 0)
  done;
  while true do
    Unix.pause ()
  done

let listen ~request_handler ~error_handler port =
  Unix.handle_unix_error (listen ~request_handler ~error_handler) port
