open Opium.Std

module Tfb_headers = struct
  let middleware =
    let open Lwt.Syntax in
    let filter handler req =
      let+ res = handler req in
      let headers = ["Server", "opium"; "Date", Opi.Time.now ()] in
      Response.add_headers_or_replace headers res
    in
    Rock.Middleware.create ~name:"TFB Headers" ~filter
end

let main () =
  let port =
    match Sys.getenv_opt "PORT" with
    | Some x -> int_of_string x
    | None -> 8080
  in
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
  let app : Opium.App.t =
    App.empty
    |> App.cmd_name "Opium"
    |> App.port port
    |> middleware Middleware.content_length
    |> middleware Tfb_headers.middleware
    |> add_routes in

  match App.run_command' app with
  | `Ok (app : unit Lwt.t ) ->
    let _ = Lwt_io.printf "Running on port 0.0.0.0:%d\n" port in
    Lwt_main.run app
  | `Error                  -> exit 1
  | `Not_running            -> exit 0

let () = main ()
