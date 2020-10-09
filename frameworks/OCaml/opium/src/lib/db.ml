module Query = struct
  type ('res, 'err) query_result = ('res, [> Caqti_error.call_or_retrieve ] as 'err) result Lwt.t

  let get_world
    : random:int -> Caqti_lwt.connection -> (Models.World.t, 'err) query_result =
    let open Models.World in
    [%rapper get_one {sql|
        SELECT @int{id}, @int{randomNumber} FROM World
        WHERE id = %int{random}
    |sql} record_out]

  let get_fortunes
    : unit -> Caqti_lwt.connection -> (Models.Fortune.t list, 'err) query_result =
    let open Models.Fortune in
    [%rapper get_many {sql|
        SELECT @int{id}, @string{message} FROM Fortunes
    |sql} record_out]

end

module Get = struct
  let get_world random : (Models.World.t, string) Lwt_result.t =
    Pool.query (Query.get_world ~random)

  let get_fortunes () : (Models.Fortune.t list, string) Lwt_result.t =
    Pool.query (Query.get_fortunes ())
end

