module Archi = struct
  let start () =
    let connection_url =
      "postgresql://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?connect_timeout=15"
    in
    Caqti_lwt.connect_pool ~max_size:10 (Uri.of_string connection_url)
    |> Result.map_error Caqti_error.show
    |> Lwt.return

  let stop
      (pool : (Caqti_lwt.connection, [> Caqti_error.connect ]) Caqti_lwt.Pool.t)
      =
    Logs.info (fun m -> m "Disconnecting from database");
    Caqti_lwt.Pool.drain pool

  let component :
      ( unit,
        (Caqti_lwt.connection, [ | Caqti_error.t ]) Caqti_lwt.Pool.t )
      Archi_lwt.Component.t =
    Archi_lwt.Component.make ~start ~stop
end

module Query = struct
  type ('res, 'err) query_result =
    ('res, ([> Caqti_error.call_or_retrieve ] as 'err)) result Lwt.t

  type ('res, 'err) query = Caqti_lwt.connection -> ('res, 'err) query_result

  let get_world : id:int -> (Models.world, 'err) query =
    let open Models in
    [%rapper
      get_one
        {sql|
        SELECT @int{id}, @int{randomNumber} FROM World
        WHERE id = %int{id}
    |sql}
        record_out]

  let get_fortunes : unit -> (Models.fortune list, 'err) query =
    let open Models in
    [%rapper
      get_many
        {sql|
        SELECT @int{id}, @string{message} FROM Fortune
    |sql}
        record_out]

  let update_random_number : random_number:int -> id:int -> (unit, 'err) query =
    [%rapper
      execute
        {sql|
        UPDATE World
        SET randomNumber = %int{random_number}
        WHERE id = %int{id}
    |sql}]
end

module Middleware = struct
  module Env = struct
    let key = Hmap.Key.create ()
  end

  let get_db (request : Morph.Request.t) = Hmap.get Env.key request.ctx

  let use request query =
    let pool = get_db request in
    Caqti_lwt.Pool.use query pool

  let get_world request (id : int) : (Models.world, string) Lwt_result.t =
    use request (Query.get_world ~id) |> Lwt_result.map_err Caqti_error.show

  let get_fortunes request : (Models.fortune list, string) Lwt_result.t =
    use request (Query.get_fortunes ()) |> Lwt_result.map_err Caqti_error.show

  let update_random_number request ~random_number id :
      (unit, string) Lwt_result.t =
    use request (Query.update_random_number ~random_number ~id)
    |> Lwt_result.map_err Caqti_error.show

  let middleware ~db (handler : Morph.Server.handler)
      (request : Morph.Request.t) =
    let ctx = Hmap.add Env.key db request.ctx in
    let next_request = { request with ctx } in
    handler next_request
end
