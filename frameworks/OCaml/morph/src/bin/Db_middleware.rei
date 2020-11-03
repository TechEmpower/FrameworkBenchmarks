let use:
  (
    Morph.Request.t,
    (module Caqti_lwt.CONNECTION) => Lwt.t(result('a, Caqti_error.t))
  ) =>
  Lwt.t(result('a, Caqti_error.t));

let middleware:
  (~db: Caqti_lwt.Pool.t(module Caqti_lwt.CONNECTION, Caqti_error.t)) =>
  Morph.Server.middleware;
