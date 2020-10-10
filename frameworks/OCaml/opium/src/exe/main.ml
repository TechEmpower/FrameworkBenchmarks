open Opium.Std

let get_count_param req = match (int_of_string (Router.param req "count")) with
  | x -> x
  | exception _e -> 1

let main () =
  let port =
    match Sys.getenv_opt "PORT" with
    | Some x -> int_of_string x
    | None -> 8080
  in
  let routes =
    [
      "/plaintext", (fun _req -> Opi.Routes.plaintext ());
      "/json", (fun _req -> Opi.Routes.json ());
      "/db", (fun _req -> Opi.Routes.db ());
      "/fortunes", (fun _req -> Opi.Routes.fortunes ());
      "/queries/", (fun _req -> Opi.Routes.queries 1);
      "/queries/:count", (fun req -> Opi.Routes.queries (get_count_param req));
      "/updates/", (fun req -> Opi.Routes.updates 1);
      "/updates/:count", (fun req -> Opi.Routes.updates (get_count_param req))
    ]
  in
  let add_routes app = List.fold_left (fun app (route,handler) -> (get route handler) app) app routes in
  let app : Opium.App.t =
    App.empty
    |> App.cmd_name "Opium"
    |> App.port port
    |> middleware Middleware.content_length
    |> middleware Tfb_headers.Middleware.m
    |> add_routes in

  match App.run_command' app with
  | `Ok (app : unit Lwt.t ) ->
    let _ = Lwt_io.printf "Running on port 0.0.0.0:%d\n" port in
    Lwt_main.run app
  | `Error                  -> exit 1
  | `Not_running            -> exit 0

let () = main ()
