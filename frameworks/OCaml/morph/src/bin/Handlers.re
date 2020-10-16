let select_random =
  Caqti_request.find(
    Caqti_type.int,
    Caqti_type.(tup2(int, int)),
    "SELECT id, randomNumber FROM World WHERE id = $1",
  );

let text = _req => {
  Morph.Response.text("Hello, World!") |> Lwt.return;
};

let json = _req => {
  let json = `Assoc([("message", `String("Hello, World!"))]);
  Yojson.Safe.to_string(json) |> Morph.Response.json |> Lwt.return;
};

let db = req => {
  open Lwt_result.Infix;
  let read_db' = (module C: Caqti_lwt.CONNECTION) =>
    C.find(select_random, Random.int(10000 + 1));
  Db_middleware.use(req, read_db')
  |> Lwt_result.map_err(e => `Server(Caqti_error.show(e)))
  >>= (
    ((id, randomNumber)) => {
      `Assoc([("id", `Int(id)), ("randomNumber", `Int(randomNumber))])
      |> Yojson.Safe.to_string
      |> Morph.Response.json
      |> Lwt.return;
    }
  );
};

let queries = (queries, req: Morph.Request.t) => {
  open Lwt.Infix;

  let query_ids = List.init(queries, _ => Random.int(10000 + 1));

  let read_query' = (x, module C: Caqti_lwt.CONNECTION) => {
    C.find(select_random, x);
  };

  let pool = Db_middleware.use(req);

  query_ids
  |> List.map(id =>
       pool(read_query'(id))
       >|= (
         fun
         | Ok((id, randomNumber)) =>
           Ok(
             `Assoc([
               ("id", `Int(id)),
               ("randomNumber", `Int(randomNumber)),
             ]),
           )
         | Error(err) => Error(err)
       )
     )
  |> Lwt.all
  >|= List.filter_map(Result.to_option)
  >|= (l => Yojson.Safe.to_string(`List(l)))
  >|= Morph.Response.json;
};
