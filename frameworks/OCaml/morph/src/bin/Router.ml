let json_route : (Morph.Server.handler, 'a) Routes.target =
  Routes.(s "json" /? nil)

let plaintext_route : (Morph.Server.handler, 'a) Routes.target =
  Routes.(s "plaintext" /? nil)

let db_route : (Morph.Server.handler, 'a) Routes.target =
  Routes.(s "db" /? nil)

let query_route_missing : (Morph.Server.handler, 'a) Routes.target =
  Routes.(s "query" /? nil)

let query_route_int =
  Routes.(s "query" / int /? nil)

let query_route_string =
  Routes.(s "query" / str /? nil)

let not_found_handler _request =
  Morph.Response.not_found () |> Lwt.return

let routes = Routes.[
  json_route @--> Json_handler.make;
  plaintext_route @--> Text_handler.make;
  db_route @--> Db_handler.make;
  query_route_missing @--> Query_handler.make 1;
  query_route_string @--> (fun _ -> Query_handler.make 1);
  query_route_int @--> (fun queries ->
    if queries > 500 then
      Query_handler.make 500
    else if queries < 1 then
      Query_handler.make 1 
    else
      Query_handler.make queries
  );
]

let handler = Morph.Router.make ~get:routes ~print:true ~not_found_handler ();
