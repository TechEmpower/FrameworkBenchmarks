let random_int = () => Random.int(10000) + 1;

let text = _req => {
  Morph.Response.text("Hello, World!") |> Lwt.return;
};

let json: Morph.Server.handler =
  _req => {
    let json = `Assoc([("message", `String("Hello, World!"))]);
    Yojson.Safe.to_string(json) |> Morph.Response.json |> Lwt.return;
  };

let db = req => {
  open Lwt_result.Infix;
  let id = random_int();
  Caqti.Middleware.get_world(req, id)
  |> Lwt_result.map_err(e => `Server(e))
  >>= (
    (Models.{id, randomNumber}) => {
      `Assoc([("id", `Int(id)), ("randomNumber", `Int(randomNumber))])
      |> Yojson.Safe.to_string
      |> Morph.Response.json
      |> Lwt.return;
    }
  );
};

let queries = (count, req: Morph.Request.t) => {
  open Lwt.Syntax;

  let query_ids = List.init(count, _ => random_int());

  let+ worlds_json =
    List.map(
      id => {
        let+ result = Caqti.Middleware.get_world(req, id);
        switch (result) {
        | Error(_) => failwith("failed to query")
        | Ok(Models.{id, randomNumber}) =>
          `Assoc([("id", `Int(id)), ("randomNumber", `Int(randomNumber))])
        };
      },
      query_ids,
    )
    |> Lwt.all;

  let json_string = Yojson.Safe.to_string(`List(worlds_json));
  Morph.Response.json(json_string);
};

let respond_html = elt => {
  Morph.Response.html(Format.asprintf("%a", Tyxml.Html.pp(), elt));
};

let fortunes = req => {
  open Lwt.Syntax;
  let+ result = Caqti.Middleware.get_fortunes(req);

  let additional_fortune =
    Models.{id: 0, message: "Additional fortune added at request time."};

  switch (result) {
  | Ok(fortunes) =>
    [additional_fortune, ...fortunes]
    |> List.sort(Models.compare_fortune)
    |> View.fortunes_page
    |> respond_html
  | Error(str) => Error(`Server(str))
  };
};
