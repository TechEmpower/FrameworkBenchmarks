let select_random =
  Caqti_request.find(
    Caqti_type.int,
    Caqti_type.(tup2(int, int)),
    "SELECT id, randomNumber FROM World WHERE id = $1",
  );

type db_error = [ | `Database_error(string)];

let or_error = (m): Lwt_result.t('a, db_error) => {
  Lwt_result.map_err(err => {`Database_error(Caqti_error.show(err))}, m);
};

let text = _req => {
  Morph.Response.text("Hello, World!") |> Lwt.return;
};

let json = _req => {
  let json = `Assoc([("message", `String("Hello, World!"))]);
  Yojson.Safe.to_string(json) |> Morph.Response.json |> Lwt.return;
};

let db = req => {
  open Lwt.Syntax;
  open Lwt.Infix;
  let read_db' = (module C: Caqti_lwt.CONNECTION) =>
    C.find(select_random, Random.int(10000 + 1));
  let+ (id, randomNumber) =
    Db_middleware.use(req, read_db')
    |> or_error
    >|= (
      fun
      | Ok((x, y)) => (x, y)
      | Error(`Database_error(str)) => failwith(str)
    );

  let json =
    `Assoc([("id", `Int(id)), ("randomNumber", `Int(randomNumber))]);
  Yojson.Safe.to_string(json) |> Morph.Response.json;
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
