module Archi : sig
  val start :
    unit ->
    ( (Caqti_lwt.connection, [> Caqti_error.connect ]) Caqti_lwt.Pool.t,
      string )
    result
    Lwt.t

  val stop :
    (Caqti_lwt.connection, [> Caqti_error.connect ]) Caqti_lwt.Pool.t ->
    unit Lwt.t

  val component :
    ( unit,
      (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t )
    Archi_lwt.Component.t
end

module Middleware : sig
  val use :
    Morph.Request.t ->
    ((module Caqti_lwt.CONNECTION) -> ('a, Caqti_error.t) Lwt_result.t) ->
    ('a, Caqti_error.t) Lwt_result.t

  val get_world : Morph.Request.t -> int -> (Models.world, string) Lwt_result.t

  val get_fortunes :
    Morph.Request.t -> (Models.fortune list, string) Lwt_result.t

  val update_random_number :
    Morph.Request.t -> random_number:int -> int -> (unit, string) result Lwt.t

  val middleware :
    db:((module Caqti_lwt.CONNECTION), Caqti_error.t) Caqti_lwt.Pool.t ->
    Morph.Server.middleware
end
