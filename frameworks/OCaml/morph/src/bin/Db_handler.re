let select_random =
  Caqti_request.find(
    Caqti_type.int,
    Caqti_type.(tup2(int, int)),
    "SELECT id, randomNumber FROM World WHERE id = $1",
  );

type error = [ | `Database_error(string)];

let or_error = m => {
  Lwt_result.map_err(err => {`Database_error(Caqti_error.show(err))}, m);
};

let make = req => {
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
