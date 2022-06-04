open Opium.Std
open Lwt.Syntax

module Tfb_headers = struct
  let memo_date = ref (Opi.Time.now ())

  let start_refreshing_date () = 
    let refresh_date _sig =
      memo_date := Opi.Time.now ();
      ignore (Unix.alarm 1)
    in
    ignore (Sys.(signal sigalrm (Signal_handle refresh_date)));
    refresh_date ()

  let middleware =
    let filter handler req =
      let+ res = handler req in
      let headers = ["Server", "opium"; "Date", !memo_date] in
      Response.add_headers_or_replace headers res
    in

    Rock.Middleware.create ~name:"TFB Headers" ~filter
end

let start_refreshing_date = Tfb_headers.start_refreshing_date

(* lwt debugging information *)
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

let create_connection_handler t addr fd=
  let f ~request_handler ~error_handler =
    Httpaf_lwt_unix.Server.create_connection_handler
      ~request_handler:(fun _ -> request_handler)
      ~error_handler:(fun _ -> error_handler)
      addr
      fd
  in
  Opium_kernel.Server_connection.run f t

let create_app ~port =
  let routes =
    [
      "/plaintext", Routes.plaintext;
      "/json", Routes.json;
      "/db", Routes.single_query;
      "/fortunes", Routes.fortunes;
      "/queries/", Routes.multiple_queries;
      "/queries/:count", Routes.multiple_queries;
      "/updates/", Routes.updates;
      "/updates/:count", Routes.updates
    ]
  in
  let add_routes app = List.fold_left (fun app (route,handler) -> (get route handler) app) app routes in

  App.empty
  |> App.cmd_name "Opium"
  |> App.port port
  |> middleware Middleware.content_length
  |> middleware Tfb_headers.middleware
  |> add_routes
