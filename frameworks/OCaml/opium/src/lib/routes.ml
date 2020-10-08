let random_int () = Random.int 10000 + 1

let plaintext () =
  let response = Opium.Std.Response.of_plain_text "Hello, World!" in
  Lwt.return response

let json () =
  let json = Models.World.message_response_to_yojson { message="Hello, World!" } in
  let response = Opium.Std.Response.of_json json in
  Lwt.return response

let db () =
  let open Lwt.Syntax in
  let random = random_int () in
  let* result = Db.Get.get_world random in
  let response = match result with
    | Error _e -> failwith "failed db"
    | Ok world -> 
      let json = Models.World.to_yojson world in
      Opium.Std.Response.of_json json in
  Lwt.return response

let queries count =
  let count = match count with
    | x when x < 1 -> 1
    | x when x > 500 -> 500
    | x -> x
  in
  let query_ids = List.init count (fun _ -> random_int ()) in
  let open Lwt.Syntax in
  let results = List.map (fun id ->
      let+ result = Db.Get.get_world id in
      match result with
      | Error _ -> failwith "failed queries"
      | Ok w -> w
    ) query_ids in
  let* results = Lwt.all results in
  let json = Models.World.list_response_to_yojson results in
  let response = Opium.Std.Response.of_json json in
  Lwt.return response

let fortunes () =
  let open Lwt.Syntax in
  let* result = Db.Get.get_fortunes () in
  let fortunes = match result with
    | Error _ -> failwith "failed fortunes"
    | Ok xs -> 
      let x = Models.Fortune.{id=0;message= "Additional fortune added at request time."} in
      x::xs |> List.sort Models.Fortune.compare
  in
  let response = Opium.Std.Response.of_html ~indent:false (Views.fortunes_page fortunes) in
  Lwt.return response
