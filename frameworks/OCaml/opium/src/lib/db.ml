module Query = struct
  type ('res, 'err) query_result = ('res, [> Caqti_error.call_or_retrieve ] as 'err) result Lwt.t
  type ('res, 'err) query = Caqti_lwt.connection -> ('res, 'err) query_result

  let get_world
    : id:int -> (Models.World.t, 'err) query =
    let open Models.World in
    [%rapper get_one {sql|
        SELECT @int{id}, @int{randomNumber} FROM World
        WHERE id = %int{id}
    |sql} record_out]

  let get_fortunes
    : unit -> (Models.Fortune.t list, 'err) query =
    let open Models.Fortune in
    [%rapper get_many {sql|
        SELECT @int{id}, @string{message} FROM Fortune
    |sql} record_out]

  let update_random_number
    : updated_random_number:int -> id:int -> (unit, 'err) query =
    [%rapper execute {sql|
        UPDATE World
        SET randomNumber = %int{updated_random_number}
        WHERE id = %int{id}
    |sql}]
end

module Get = struct
  let get_world id : (Models.World.t, string) Lwt_result.t =
    Pool.query (Query.get_world ~id)

  let get_fortunes () : (Models.Fortune.t list, string) Lwt_result.t =
    Pool.query (Query.get_fortunes ())
end

module Update = struct
  type update_random_numbers_req = { updated_random_number:int; id:int }

  let update_world {updated_random_number; id} =
    Pool.query (Query.update_random_number ~updated_random_number ~id)
end

