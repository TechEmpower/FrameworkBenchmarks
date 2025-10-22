open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type message_object = {
  message : string;
} [@@deriving yojson]

let time_cache = Atomic.make false;;
let time_ref = ref("None");;

let time () = 
    if not @@ Atomic.get time_cache 
    then begin
        time_ref := CalendarLib.Printer.Calendar.sprint "%a, %d %b %Y %H:%M:%S UTC" @@ CalendarLib.Calendar.now ();
        Atomic.set time_cache true;
        (!time_ref)
        end 
    else 
        (!time_ref);;

let tech_empower_headers (inner_handler: Dream.handler) =
    (fun (req) ->
        let%lwt res = inner_handler req in
        Dream.set_header res "Server" "dream";
        Dream.set_header res "Date" @@ time (); 
        Lwt.return res
    );;

let rec timer () =
    Unix.sleepf 0.9;
    Atomic.set time_cache false;
    timer();;

let () =
    let time_invalidator = Domain.spawn(fun () -> timer ()) in
    Dream.run ~interface: "0.0.0.0"
    @@ tech_empower_headers
    @@ Dream.router [
        Dream.get "/" (fun _ ->
            Dream.html "Hello, world!"
        );
        Dream.get "/plaintext" (fun _ ->
            Dream.response ~headers: [("Content-Type", "text/plain")]
                "Hello, world!"
            |> Lwt.return
        );
        Dream.get "/json" (fun _ ->
            { message = "Hello, world!" }
            |> yojson_of_message_object
            |> Yojson.Safe.to_string
            |> Dream.json
        );
    ];
    Domain.join time_invalidator;;
