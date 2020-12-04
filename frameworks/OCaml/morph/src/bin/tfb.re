let error_handler = (_client_addr, ~request as _=?, ~respond, err) => {
  let error_to_string =
    fun
    | `Bad_gateway => "Bad gateway"
    | `Bad_request => "Bad request"
    | `Exn(_exn) => "Unhandled server error"
    | `Internal_server_error => "Internal server error";

  let error_handler =
    respond(
      ~headers=Piaf.Headers.of_list([("connection", "close")]),
      Piaf.Body.of_string(error_to_string(err)),
    );

  Lwt.return(error_handler);
};

module WebServer = {
  let start = ((), db) => {
    let port =
      switch (Sys.getenv_opt("PORT")) {
      | Some(p) => int_of_string(p)
      | None => 8080
      };

    Logs.app(m => m("Starting server on %n", port));

    let request_handler =
      Headers_middleware.make(
        Caqti.Middleware.middleware(~db, Router.handler),
      )
      |> Morph_wrapper.wrap_context;
    Server_io.listen(~request_handler, ~error_handler, port)
    |> Lwt_result.return;
  };

  let stop = _server => {
    Logs.info(m => m("Stopped Server")) |> Lwt.return;
  };

  let component: Archi_lwt.Component.t(unit, unit) =
    Archi_lwt.Component.using(
      ~start,
      ~stop,
      ~dependencies=[Caqti.Archi.component],
    );
};

let system =
  Archi_lwt.System.make([
    ("database", Caqti.Archi.component),
    ("web_server", WebServer.component),
  ]);

let main = () => {
  open Lwt.Infix;
  Logger.setup_log(Some(Logs.Warning));

  Archi_lwt.System.start((), system)
  >|= (
    fun
    | Ok(system) => {
        Logs.info(m => m("Starting"));

        Sys.(
          set_signal(
            sigint,
            Signal_handle(
              _ => {
                Logs.err(m => m("SIGNINT received, tearing down.@."));
                Archi_lwt.System.stop(system) |> ignore;
              },
            ),
          )
        );
      }
    | Error(error) => {
        Logs.err(m => m("ERROR: %s@.", error));
        exit(1);
      }
  );
};

Lwt_engine.set((new Lwt_engine.libev)());

let () = Lwt_main.run(main());
