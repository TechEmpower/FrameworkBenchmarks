let json_route : (Morph.Server.handler, 'a) Routes.target =
  Routes.(s "json" /? nil)

let plaintext_route : (Morph.Server.handler, 'a) Routes.target =
  Routes.(s "plaintext" /? nil)

let db_route : (Morph.Server.handler, 'a) Routes.target = Routes.(s "db" /? nil)

let query_route_missing : (Morph.Server.handler, 'a) Routes.target =
  Routes.(s "queries" //? nil)

let query_route_int = Routes.(s "queries" / int /? nil)

let query_route_str = Routes.(s "queries" / str /? nil)

let fortunes_route = Routes.(s "fortunes" /? nil)

let not_found_handler _request = Morph.Response.not_found () |> Lwt.return

let routes =
  Routes.
    [
      json_route @--> Handlers.json;
      plaintext_route @--> Handlers.text;
      db_route @--> Handlers.db;
      query_route_missing @--> Handlers.queries 1;
      (query_route_int @--> function
       | queries when queries > 500 -> Handlers.queries 500
       | queries when queries < 1 -> Handlers.queries 1
       | queries -> Handlers.queries queries);
      (query_route_str @--> fun _ -> Handlers.queries 1);
      fortunes_route @--> Handlers.fortunes;
    ]

let handler = Morph.Router.make ~get:routes ~print:true ~not_found_handler ()
