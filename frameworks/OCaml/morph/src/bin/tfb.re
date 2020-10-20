module Database = {
  let start =
      ()
      : Lwt.t(
          result(
            Caqti_lwt.Pool.t(module Caqti_lwt.CONNECTION, Caqti_error.t),
            string,
          ),
        ) => {
    let connection_url = "postgresql://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?connect_timeout=15";

    Caqti_lwt.connect_pool(~max_size=10, Uri.of_string(connection_url))
    |> (
      fun
      | Ok(pool) => Ok(pool)
      | Error(error) => Error(Caqti_error.show(error))
    )
    |> Lwt.return;
  };

  let stop = _pool => {
    Logs.info(m => m("Disconnected from database")) |> Lwt.return;
  };

  let component = Archi_lwt.Component.make(~start, ~stop);
};

module WebServer = {
  let port =
    try(Sys.getenv("PORT") |> int_of_string) {
    | _ => 8080
    };

  let server = Morph.Server.make(~port, ~address=Unix.inet_addr_any, ());

  let start = ((), db) => {
    Logs.app(m => m("Starting server on %n", port));

    let handler =
      Headers_middleware.make(Db_middleware.middleware(~db, Router.handler));

    server.start(handler) |> Lwt_result.ok;
  };

  let stop = _server => {
    Logs.info(m => m("Stopped Server")) |> Lwt.return;
  };

  let component =
    Archi_lwt.Component.using(
      ~start,
      ~stop,
      ~dependencies=[Database.component],
    );
};

let system =
  Archi_lwt.System.make([
    ("database", Database.component),
    ("web_server", WebServer.component),
  ]);

let main = () => {
  open Lwt.Infix;
  Logger.setup_log(Some(Logs.Info));

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
