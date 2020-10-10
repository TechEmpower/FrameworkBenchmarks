let make = (queries, req: Morph.Request.t) => {
  open Lwt.Infix;

  let query_ids = List.init(queries, _ => Random.int(10000 + 1));

  let read_query' = (x, module C: Caqti_lwt.CONNECTION) => {
    C.find(Db_handler.select_random, x);
  };

  let pool = Db_middleware.use(req);

  query_ids
  |> List.map(id =>
       pool(read_query'(id))
       |> Db_handler.or_error
       >|= (
         fun
         | Ok((x, y)) => (x, y)
         | Error(`Database_error(err)) => failwith(err)
       )
     )
  |> Lwt.all
  >|= List.map(((id, randomNumber)) => {
        `Assoc([("id", `Int(id)), ("randomNumber", `Int(randomNumber))])
      })
  >|= (l => Yojson.Safe.to_string(`List(l)))
  >|= Morph.Response.json;
};
