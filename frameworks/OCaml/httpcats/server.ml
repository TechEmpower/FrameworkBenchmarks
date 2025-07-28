(* Dates *)

let get_date () = Unix.(gettimeofday () |> gmtime)

let dow = function
  | 0 -> "Sun"
  | 1 -> "Mon"
  | 2 -> "Tue"
  | 3 -> "Wed"
  | 4 -> "Thu"
  | 5 -> "Fri"
  | _ -> "Sat"

let month = function
  | 0 -> "Jan"
  | 1 -> "Feb"
  | 2 -> "Mar"
  | 3 -> "Apr"
  | 4 -> "May"
  | 5 -> "Jun"
  | 6 -> "Jul"
  | 7 -> "Aug"
  | 8 -> "Sep"
  | 9 -> "Oct"
  | 10 -> "Nov"
  | _ -> "Dec"

let date () =
  let d = get_date () in
  (* Wed, 17 Apr 2013 12:00:00 GMT *)
  Format.sprintf "%s, %02d %s %4d %02d:%02d:%02d GMT" (dow d.tm_wday) d.tm_mday
    (month d.tm_mon) (1900 + d.tm_year) d.tm_hour d.tm_min d.tm_sec

(* HTTP *)

let _plaintext reqd =
  let open H1 in
  let payload = "Hello, World!" in
  let headers =
    Headers.of_rev_list 
      [ ("content-length", string_of_int (String.length payload))
      ; ("content-type", "text/plain")
      ; ("server", "httpcats")
      ; ("date", date ()) ] in
  let resp = Response.create ~headers `OK in
  Reqd.respond_with_string reqd resp payload

let _json reqd  =
  let open H1 in
  let obj = `Assoc [ ("message", `String "Hello, World!") ] in
  let payload = Yojson.to_string obj in
  let headers =
    Headers.of_rev_list 
      [ ("content-length", string_of_int (String.length payload))
      ; ("content-type", "application/json")
      ; ("server", "httpcats")
      ; ("date", date ()) ] in
  let resp = Response.create ~headers `OK in
  Reqd.respond_with_string reqd resp payload

let _not_found reqd =
  let open H1 in
  let moo = "m00." in
  let headers =
    Headers.of_rev_list
    [ "content-length", string_of_int (String.length moo) ] in
  let resp = Response.create ~headers `OK in
  Reqd.respond_with_string reqd resp moo

let[@warning "-8"] handler _
    (`V1 reqd : [ `V1 of H1.Reqd.t | `V2 of H2.Reqd.t ]) =
  let open H1 in
  let request = Reqd.request reqd in
  match request.Request.target with
  | "/plaintext" -> _plaintext reqd
  | "/json" -> _json reqd
  | _ -> _not_found reqd

let localhost_8080 = Unix.(ADDR_INET (inet_addr_any, 8080))

let server stop =
  Httpcats.Server.clear ~parallel:false ~stop ~backlog:4096 ~handler localhost_8080

let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let run () =
  let domains =
    Unix.open_process_in "getconf _NPROCESSORS_ONLN"
    |> input_line |> int_of_string in
  Miou_unix.run ~domains @@ fun () ->
  let stop = Httpcats.Server.stop () in
  let fn _sigint = Httpcats.Server.switch stop in
  ignore (Miou.sys_signal Sys.sigint (Sys.Signal_handle fn));
  let domains = Miou.Domain.available () in
  let prm = Miou.async @@ fun () -> server stop in
  if domains > 0 then
    Miou.parallel server (List.init domains (Fun.const stop))
    |> List.iter (function Ok () -> () | Error exn -> raise exn);
  Miou.await_exn prm

let () = Unix.handle_unix_error run ()
