(* https://github.com/ocsigen/lwt/blob/d7fabaa077389a0035254e66459a6a366c57576e/src/core/lwt_result.ml#L88-L91 *)
(* >>= is Lwt.Infix equivalent to Lwt.bind:
   https://ocsigen.org/lwt/5.3.0/api/Lwt#3_Callbacks *)
(* >|= is Lwt.Infix equivalent to Lwt.map:
   https://ocsigen.org/lwt/5.3.0/api/Lwt#2_Convenience *)
open Lwt.Infix
open Lwt.Syntax [@@ocaml.warning "-33"]
open Cohttp_lwt_unix

module Wm = struct
  module Rd = Webmachine.Rd

  module UnixClock = struct
    let now () = 0
  end

  include Webmachine.Make (Cohttp_lwt_unix__Io) (UnixClock)
end

let pool =
  let connection_url =
    "postgresql://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?connect_timeout=15"
  in
  match Caqti_lwt.connect_pool ~max_size:4 (Uri.of_string connection_url) with
  | Ok pool -> pool
  | Error err ->
      Printf.eprintf "%s" (Caqti_error.show err);
      flush stderr;
      failwith (Caqti_error.show err)

type error = Database_error of string

let or_error m =
  match%lwt m with
  | Ok a -> Ok a |> Lwt.return
  | Error err ->
      Printf.eprintf "%s" (Caqti_error.show err);
      flush stderr;
      Error (Database_error (Caqti_error.show err)) |> Lwt.return

let select_random =
  Caqti_request.find Caqti_type.int
    Caqti_type.(tup2 int int)
    "SELECT id, randomNumber FROM World WHERE id = $1"

class hello =
  object (self)
    inherit [Cohttp_lwt.Body.t] Wm.resource

    method! allowed_methods rd = Wm.continue [ `GET ] rd

    method content_types_provided rd =
      Wm.continue
        [ ("text/plain", self#to_text); ("application/json", self#to_json) ]
        rd

    method content_types_accepted rd = Wm.continue [] rd

    method private to_text rd =
      let text = "Hello, World!" in
      Wm.continue (`String text) rd

    method private to_json rd =
      let json = Lib.Message_t.{ message = "Hello, World!" } in
      Wm.continue (`String (Lib.Message_j.string_of_message json)) rd
  end

class db =
  object (self)
    inherit [Cohttp_lwt.Body.t] Wm.resource

    method! allowed_methods rd = Wm.continue [ `GET ] rd

    method content_types_provided rd =
      Wm.continue [ ("application/json", self#read_db) ] rd

    method content_types_accepted rd = Wm.continue [] rd

    method private read_db rd =
      let read_db' (module C : Caqti_lwt.CONNECTION) =
        C.find select_random (Random.int 10000 + 1)
      in
      let%lwt id, randomNumber =
        Caqti_lwt.Pool.use read_db' pool |> or_error >|= function
        | Ok (x, y) -> (x, y)
        | Error _ -> failwith "whoops"
      in
      let json =
        Lib.Db_t.
          { id = float_of_int id; randomNumber = float_of_int randomNumber }
      in
      Wm.continue (`String (Lib.Db_j.string_of_query json)) rd
  end

class queries =
  object (self)
    inherit [Cohttp_lwt.Body.t] Wm.resource

    method private id rd =
      try
        let _id = int_of_string (Wm.Rd.lookup_path_info_exn "id" rd) in
        match _id with
        | x when x < 1 -> 1
        | x when x > 500 -> 500
        | x -> x
      with
      | Failure _ -> 1
      | Not_found -> 1

    method! allowed_methods rd = Wm.continue [ `GET ] rd

    method content_types_provided rd =
      Wm.continue [ ("application/json", self#read_query) ] rd

    method content_types_accepted rd = Wm.continue [] rd

    method private read_query rd =
      let query_ids = List.init (self#id rd) (fun _ -> Random.int 10000 + 1) in
      let read_query' x (module C : Caqti_lwt.CONNECTION) =
        C.find select_random x
      in
      let response =
        List.map
          (fun id ->
            Caqti_lwt.Pool.use (read_query' id) pool |> or_error >|= function
            | Ok (x, y) -> (x, y)
            | Error _ -> failwith "whoops")
          query_ids
      in
      let%lwt resolved_response = Lwt.all response in
      let json =
        List.map
          (fun tup ->
            let id, randomNumber = tup in
            Lib.Db_t.
              { id = float_of_int id; randomNumber = float_of_int randomNumber })
          resolved_response
      in
      Wm.continue (`String (Lib.Db_j.string_of_queries json)) rd
  end

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
  Lwt.async (fun () -> Lwt_io.eprintf "Lwt:\n");
  List.iter
    (fun (str, opt) ->
      Lwt.async (fun () -> Lwt_io.eprintf "  %s = %b\n" str (Lwt_sys.have opt)))
    options

let main () =
  (* https://github.com/mirage/ocaml-cohttp/issues/328#issuecomment-222583580 *)
  Lwt_io.set_default_buffer_size 0x10000;
  let nproc =
    match Sys.getenv "APP_INSTANCES" with
    | x -> int_of_string x
    | exception Not_found ->
        Unix.open_process_in "getconf _NPROCESSORS_ONLN"
        |> input_line
        |> int_of_string
  in
  Lwt.async (fun () -> Lwt_io.eprintf "Detected %d cores\n" nproc);
  let ulimit_n =
    Unix.open_process_in "ulimit -n" |> input_line |> int_of_string
  in
  Lwt.async (fun () -> Lwt_io.eprintf "Detected %d max open files\n" ulimit_n);
  dump_lwt ();
  let port =
    match Sys.getenv "PORT" with
    | x -> int_of_string x
    | exception Not_found -> 8080
  in
  let routes =
    [
      ("/plaintext", fun () -> new hello);
      ("/json", fun () -> new hello);
      ("/db", fun () -> new db);
      ("/queries", fun () -> new queries);
      ("/queries/:id", fun () -> new queries);
    ]
  in
  let callback (_ch, _conn) request body =
    let open Cohttp in
    (Wm.dispatch' routes ~body ~request >|= function
     | None -> (`Not_found, Header.init (), `String "Not found", [])
     | Some result -> result)
    >>= fun (status, headers, body, _) ->
    let headers = Header.add headers "Server" "webmachine" in
    let headers = Header.add headers "Date" Lib.Time.(!memo_date) in
    Server.respond ~headers ~body ~status ()
  in

  let ipaddr = Unix.inet_addr_any in
  let sockaddr = Unix.ADDR_INET (ipaddr, port) in
  let socket =
    Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
  in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;

  Lwt_main.run
  @@ ( Lwt_unix.bind socket sockaddr >|= fun () ->
       Lwt_unix.listen socket (Lwt_unix.somaxconn () [@ocaml.warning "-3"]) );

  for i = 1 to nproc do
    Lwt.async (fun () -> Lwt_io.flush_all ());
    if Lwt_unix.fork () = 0 then (
      (* child *)
      Lib.Time.refresh_date ();
      Lwt.async (fun () ->
          Lwt_io.eprintf "Listening on %s:%d (child %d)\n"
            (Unix.string_of_inet_addr ipaddr)
            port i);
      let config = Server.make ~callback () in
      Lwt.async (fun () -> Server.create ~mode:(`TCP (`Socket socket)) config);
      let forever, _ = Lwt.wait () in
      Lwt_main.run forever;
      exit 0 )
  done;
  while true do
    Unix.pause ()
  done

let () = Unix.handle_unix_error main ()
