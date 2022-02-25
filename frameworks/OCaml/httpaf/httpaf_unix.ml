open Lwt.Infix
open Httpaf
open Httpaf_lwt_unix

(* Dates *)

let get_date () = Unix.(gettimeofday () |> gmtime)

let dow = function
  | 0 -> "Sun"
  | 1 -> "Mon"
  | 2 -> "Tue"
  | 3 -> "Wed"
  | 4 -> "Thu"
  | 5 -> "Fri"
  | _ -> "Sat"

let month = function
  | 0 -> "Jan"
  | 1 -> "Feb"
  | 2 -> "Mar"
  | 3 -> "Apr"
  | 4 -> "May"
  | 5 -> "Jun"
  | 6 -> "Jul"
  | 7 -> "Aug"
  | 8 -> "Sep"
  | 9 -> "Oct"
  | 10 -> "Nov"
  | _ -> "Dec"

let date () =
  let d = get_date () in
  (* Wed, 17 Apr 2013 12:00:00 GMT *)
  Format.sprintf "%s, %02d %s %4d %02d:%02d:%02d GMT" (dow d.tm_wday) d.tm_mday
    (month d.tm_mon) (1900 + d.tm_year) d.tm_hour d.tm_min d.tm_sec

let memo_date = ref @@ date ()

let refresh_date () =
  let f _ =
    memo_date := date ();
    ignore @@ Unix.alarm 1
  in
  (ignore @@ Sys.(signal sigalrm (Signal_handle f)));
  f ()

(* HTTP *)

let request_handler (_ : Unix.sockaddr) reqd =
  let req = Reqd.request reqd in
  match req.target with
  | "/json" ->
      let obj = `Assoc [ ("message", `String "Hello, World!") ] in
      let payload = Yojson.to_string obj in
      let headers =
        Headers.of_rev_list
          [
            ("content-length", string_of_int @@ String.length payload);
            ("content-type", "application/json");
            ("server", "httpaf");
            ("date", !memo_date);
          ]
      in
      let rsp = Response.create ~headers `OK in
      Reqd.respond_with_string reqd rsp payload
  | "/plaintext" ->
      let payload = "Hello, World!" in
      let headers =
        Headers.of_rev_list
          [
            ("content-length", string_of_int @@ String.length payload);
            ("content-type", "text/plain");
            ("server", "httpaf");
            ("date", !memo_date);
          ]
      in
      let rsp = Response.create ~headers `OK in
      Reqd.respond_with_string reqd rsp payload
  | _ ->
      let moo = "m00." in
      let headers =
        Headers.of_list
          [ ("content-length", string_of_int @@ String.length moo) ]
      in
      let rsp = Response.create ~headers `OK in
      Reqd.respond_with_string reqd rsp moo

let error_handler (_ : Unix.sockaddr) ?request:_ error handle =
  let message =
    match error with
    | `Exn exn -> Printexc.to_string exn
    | (#Status.client_error | #Status.server_error) as error ->
        Status.to_string error
  in
  let body = handle Headers.empty in
  Body.write_string body message;
  Body.close_writer body

let rec accept_loop socket handler =
  Lwt_unix.accept socket >>= fun (socket', sockaddr') ->
  Lwt.async (fun () ->
      Lwt.catch
        (fun () -> handler sockaddr' socket')
        (fun exn ->
          !Lwt.async_exception_hook exn;
          Lwt.return_unit));
  accept_loop socket handler

let dump_lwt () =
  let options =
    [
      ("fd_passing", `fd_passing);
      ("fdatasync", `fdatasync);
      ("get_affinity", `get_affinity);
      ("get_cpu", `get_cpu);
      ("get_credentials", `get_credentials);
      ("libev", `libev);
      ("madvise", `madvise);
      ("mincore", `mincore);
      ("recv_msg", `recv_msg);
      ("send_msg", `send_msg);
      ("set_affinity", `set_affinity);
      ("wait4", `wait4);
    ]
  in
  Printf.eprintf "Lwt:\n";
  List.iter
    (fun (str, opt) -> Printf.eprintf "  %s = %b\n" str (Lwt_sys.have opt))
    options

let main () =
  let nproc =
    Unix.open_process_in "getconf _NPROCESSORS_ONLN"
    |> input_line |> int_of_string
  in
  Printf.eprintf "Detected %d cores\n" nproc;
  let ulimit_n =
    Unix.open_process_in "ulimit -n" |> input_line |> int_of_string
  in
  Printf.eprintf "Detected %d max open files\n" ulimit_n;
  dump_lwt ();

  let ipaddr = Unix.inet_addr_any in
  let port = 8080 in
  let sockaddr = Unix.ADDR_INET (ipaddr, port) in
  let socket =
    Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
  in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;

  Lwt_main.run
  @@ ( Lwt_unix.bind socket sockaddr >|= fun () ->
       Lwt_unix.listen socket (Lwt_unix.somaxconn () [@ocaml.warning "-3"]) );

  for i = 1 to nproc do
    flush_all ();
    if Lwt_unix.fork () = 0 then (
      (* child *)
      refresh_date ();
      (Lwt.async_exception_hook := fun exn -> raise exn);
      Lwt.async (fun () ->
          Lwt_io.eprintf "Listening on %s:%s (child %d)\n"
            (Unix.string_of_inet_addr ipaddr)
            (string_of_int port) i
          >>= fun () ->
          let handler =
            Server.create_connection_handler ~request_handler ~error_handler
          in
          accept_loop socket handler);
      let forever, _ = Lwt.wait () in
      Lwt_main.run forever;
      exit 0 )
  done;

  while true do
    Unix.pause ()
  done

let () = Unix.handle_unix_error main ()
