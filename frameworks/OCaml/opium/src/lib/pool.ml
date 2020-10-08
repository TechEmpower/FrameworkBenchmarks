let url = "localhost"
let port = 5432
let database = "opi"
let connection_uri = Printf.sprintf "postgresql://%s:%i/%s" url port database

let connect () =
  connection_uri
  |> Uri.of_string
  |> Caqti_lwt.connect_pool ~max_size:10
  |> function | Ok pool   -> pool
              | Error err -> failwith (Caqti_error.show err)

let pool =
  let connection_url =
    "postgresql://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?connect_timeout=15"
  in
  match Caqti_lwt.connect_pool ~max_size:10 (Uri.of_string connection_url) with
  | Ok pool -> pool
  | Error err ->
    Printf.eprintf "%s" (Caqti_error.show err);
    flush stderr;
    failwith (Caqti_error.show err)

let query_pool query pool =
  Caqti_lwt.Pool.use query pool
  |> Lwt_result.map_err Caqti_error.show

let query q = query_pool q pool
