let weekday_to_string = function
  | `Mon -> "Mon"
  | `Tue -> "Tue"
  | `Wed -> "Wed"
  | `Thu -> "Thu"
  | `Fri -> "Fri"
  | `Sat -> "Sat"
  | `Sun -> "Sun"

let month_to_string = function
  | 1 -> "Jan"
  | 2 -> "Feb"
  | 3 -> "Mar"
  | 4 -> "Apr"
  | 5 -> "May"
  | 6 -> "Jun"
  | 7 -> "Jul"
  | 8 -> "Aug"
  | 9 -> "Sep"
  | 10 -> "Oct"
  | 11 -> "Nov"
  | 12 -> "Dec"
  | _ -> failwith "month"

let timestamp now =
  let weekday_str = weekday_to_string (Ptime.weekday now) in
  let (year, month, day), ((hour, minute, second), _) =
    Ptime.to_date_time now
  in
  let month_str = month_to_string month in
  Printf.sprintf "%s, %02u %s %04u %02u:%02u:%02u GMT" weekday_str day month_str
    year hour minute second

let make (handler : Morph.Server.handler) request =
  let open Lwt.Infix in
  handler request
  >|= Morph.Response.add_headers
        [ ("Date", timestamp (Ptime_clock.now ())); ("Server", "Morph") ]
