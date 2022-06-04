let weekday Unix.{tm_wday;_} = match tm_wday with
  | 0 -> "Sun"
  | 1 -> "Mon"
  | 2 -> "Tue"
  | 3 -> "Wed"
  | 4 -> "Thu"
  | 5 -> "Fri"
  | 6 -> "Sat"
  | _ -> failwith "weekday"

let month Unix.{tm_mon;_} = match tm_mon with
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
  | 11 -> "Dec"
  | _ -> failwith "month"

let gmt tm =
  Printf.sprintf "%s, %02u %s %04u %02u:%02u:%02u GMT" (weekday tm) tm.tm_mday (month tm) (tm.tm_year + 1900)  tm.tm_hour tm.tm_min tm.tm_sec

let now () = (gmt (Unix.gmtime (Unix.gettimeofday ())))
