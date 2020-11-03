module Env = {
  let key = Hmap.Key.create();
};

let get_db = (request: Morph.Request.t) => Hmap.get(Env.key, request.ctx);

let use = (request, query) => {
  let pool = get_db(request);
  Caqti_lwt.Pool.use(query, pool);
};

let middleware:
  (~db: Caqti_lwt.Pool.t(module Caqti_lwt.CONNECTION, 'e)) =>
  Morph.Server.middleware =
  (~db, handler, request) => {
    let next_request = {...request, ctx: Hmap.add(Env.key, db, request.ctx)};
    handler(next_request);
  };
