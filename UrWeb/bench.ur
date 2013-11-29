open Json

fun addHeaders () =
  n <- now;
  setHeader (blessResponseHeader "Date") (timef "%a, %d %b %Y %H:%M:%S GMT" n);
  setHeader (blessResponseHeader "Server") "Ur/Web"
fun clamp n =
  (n % 10000) + 1
fun parseQueries oqs : int =
  let
    val qt = case oqs of
        None => Some ("queries", "1")
      | Some qs => String.split (show qs) #"="
    val on = case qt of
        Some ("queries", x) => read x
      | _ => Some 1
  in
    case on of
      None => 1
    | Some x => if x > 500 then 500
                else if x < 1 then 1
                else x
  end
fun returnJson [a] (_ : json a) (j : a) : transaction page =
  addHeaders ();
  returnBlob (textBlob (toJson j)) (blessMime "application/json")
fun returnText (t : string) : transaction page =
  addHeaders ();
  returnBlob (textBlob t) (blessMime "text/plain")

val hello = "Hello, World!"
fun plaintext () =
  returnText hello

type json_t = {Message : string}
val json_conversion : json json_t = json_record {Message = "message"}
val hello_json = {Message = hello}
fun json () =
    returnJson hello_json

table world : {Id : int, RandomNumber : int} PRIMARY KEY Id
type world_t = {Id : int, RandomNumber : int}
val world_conversion : json world_t = json_record {Id = "id", RandomNumber = "randomNumber"}
fun world_find n =
  oneRow1 (SELECT World.Id, World.RandomNumber FROM world WHERE World.Id = {[n]})

fun db () =
  n <- rand;
  row <- world_find (clamp n);
  returnJson row

fun queries oqs =
  rows <- List.tabulateM (fn _ => n <- rand; world_find (clamp n)) (parseQueries oqs);
  returnJson rows

fun updates oqs =
  rows <- List.tabulateM (fn _ => n <- rand; world_find (clamp n)) (parseQueries oqs);
  rows' <- List.mapM (fn r => n <- rand; return (r -- #RandomNumber ++ {RandomNumber = clamp n})) rows;
  List.app (fn r => dml (UPDATE world SET RandomNumber = {[r.RandomNumber]} WHERE Id = {[r.Id]})) rows';
  returnJson rows'

table fortune : {Id : int, Message : string} PRIMARY KEY Id
type fortune_t = {Id : int, Message : string}
val fortune_conversion : json fortune_t = json_record {Id = "id", Message = "message"}
val new_fortune : fortune_t = {Id = 0, Message = "Additional fortune added at request time"}
fun fortunes () =
  fs <- queryL1 (SELECT Fortune.Id, Fortune.Message FROM fortune);
  let
    val fs' = List.sort (fn x y => x.Message > y.Message ) (new_fortune :: fs)
  in
    addHeaders ();
    return <xml>
             <head><title>Fortunes</title></head>
             <body><table>
               <tr><th>id</th><th>message</th></tr>
               {List.mapX (fn f => <xml><tr>
                 <td>{[f.Id]}</td><td>{[f.Message]}</td>
               </tr></xml>) fs'}
             </table></body>
           </xml>
  end
