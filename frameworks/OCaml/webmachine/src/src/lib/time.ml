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

let memo_date = ref @@ date ()

let refresh_date () =
  let f _ =
    memo_date := date ();
    ignore @@ Unix.alarm 1
  in
  (ignore @@ Sys.(signal sigalrm (Signal_handle f)));
  f ()
