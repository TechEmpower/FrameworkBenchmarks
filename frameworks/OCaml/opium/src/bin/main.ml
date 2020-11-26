open Opium.Std

let main () =
  let port =
    match Sys.getenv_opt "PORT" with
    | Some x -> int_of_string x
    | None -> 8080
  in

  Server.dump_lwt ();

  let app = Server.create_app ~port in

  Server.start_refreshing_date ();

  match App.run_command' app with
  | `Ok (app : unit Lwt.t ) ->
    let _ = Lwt_io.printf "Running on port 0.0.0.0:%d\n" port in
    Lwt_main.run app
  | `Error                  -> exit 1
  | `Not_running            -> exit 0

let () = main ()
