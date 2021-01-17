let random_int () = Random.int 10000 + 1

let plaintext () = "Hello, World!" 

let json () = Models.World.message_response_to_yojson { message="Hello, World!" }

let single_query () =
  let open Lwt.Syntax in
  let id = random_int () in
  let+ result = Db.Get.get_world id in
  match result with
  | Error _e -> failwith "failed db"
  | Ok world -> Models.World.to_yojson world

let get_worlds count =
  let count = match count with
    | x when x < 1 -> 1
    | x when x > 500 -> 500
    | x -> x
  in
  let query_ids = List.init count (fun _ -> random_int ()) in
  let open Lwt.Syntax in
  query_ids |> List.map (fun id ->
      let+ result = Db.Get.get_world id in
      match result with
      | Error _ -> failwith "failed queries"
      | Ok w -> w
    )

let multiple_queries count =
  let open Lwt.Syntax in
  let+ worlds = Lwt.all (get_worlds count) in
  Models.World.list_response_to_yojson worlds

let updates count =
  let open Lwt.Syntax in
  let* worlds = Lwt.all (get_worlds count) in
  let results = worlds |> List.map (fun (world: Models.World.t) ->
      let updated_random_number = random_int () in
      let updated_world_req = Db.Update.{updated_random_number;id=world.id} in
      let+ result = Db.Update.update_world updated_world_req in
      match result with
      | Error _ -> failwith "failed queries"
      | Ok () -> {world with randomNumber=updated_random_number}
    ) in
  let+ updated_worlds = Lwt.all results in
  Models.World.list_response_to_yojson updated_worlds

let fortunes () =
  let open Lwt.Syntax in
  let+ result = Db.Get.get_fortunes () in
  let fortunes = match result with
    | Error _ -> failwith "failed fortunes"
    | Ok xs -> 
      let x = Models.Fortune.{id=0;message= "Additional fortune added at request time."} in
      x::xs |> List.sort Models.Fortune.compare
  in
  Views.fortunes_page fortunes

