open Json

fun addHeaders () =
  n <- now;
  setHeader (blessResponseHeader "Date") (timef "%a, %d %b %Y %H:%M:%S GMT" n);
  setHeader (blessResponseHeader "Server") "Ur/Web"


val hello = "Hello, World!"
fun plaintext () =
  addHeaders ();
  returnBlob (textBlob hello) (blessMime "text/plain")


type json_t = {Message : string}
fun json () =
  let
    val json_conversion : json json_t = json_record {Message = "message"}
    val hello_json : json_t = {Message = hello}
  in
    addHeaders ();
    returnBlob (textBlob (toJson hello_json)) (blessMime "application/json")
  end

table world : {Id : int, RandomNumber : int} PRIMARY KEY Id
type world_t = {Id : int, RandomNumber : int}
val world_conversion : json world_t = json_record {Id = "id", RandomNumber = "randomNumber"}
fun world_find n =
  oneRow1 (SELECT World.Id, World.RandomNumber FROM world WHERE World.Id = {[n]})

fun clamp n =
  (mod n 10000) + 1

fun db () =
  addHeaders ();
  n <- rand;
  row <- world_find (clamp n);
  returnBlob (textBlob (toJson row)) (blessMime "application/json")

fun parseQueries oqs =
  let
    val qt = case oqs of
        None => Some ("queries", "1")
      | Some qs => String.split (show qs) #"="
    val on = case qt of
        None => Some 1
      | Some ("queries", x) => read x
      | Some _ => Some 1
  in
    case on of
      None => 1
    | Some x => if x > 500 then 500
                else if x < 1 then 1
                else x
  end

fun range n acc =
  case n of
      0 => acc
    | _ => range (n-1) (n :: acc)

fun queries oqs =
  addHeaders ();
  let
    val rq = range (parseQueries oqs) []
  in
    rands <- List.mapM (fn _ => rand) rq;
    rows <- List.mapM (fn r => world_find (clamp r)) rands;
    returnBlob (textBlob (toJson rows)) (blessMime "application/json")
  end

table fortune : {Id : int, Message : string} PRIMARY KEY Id
type fortune_t = {Id : int, Message : string}
fun fortunes () =
  addHeaders ();
  fs <- queryL1 (SELECT Fortune.Id, Fortune.Message FROM fortune);
  let
    val fortune_conversion : json fortune_t = json_record {Id = "id", Message = "message"}
    val new_fortune : fortune_t = {Id = 0, Message = "Additional fortune added at request time"}
    val fs' = List.sort (fn x y => x.Message > y.Message ) (new_fortune :: fs)
    val tabled = List.mapX (fn row =>
      <xml>
        <tr><td>{[row.Id]}</td><td>{[row.Message]}</td></tr>
      </xml>) fs'
  in
    return <xml>
             <head><title>Fortunes</title></head>
             <body><table>
               <tr><th>id</th><th>message</th></tr>
               {tabled}
             </table></body>
           </xml>
  end

fun updates oqs =
  addHeaders ();
  let
    fun map2 f ls1 ls2 =
      case (ls1, ls2) of
        ([], []) => []
      | (x1 :: ls1, x2 :: ls2) => (f x1 x2) :: map2 f ls1 ls2
      | _ => error <xml>map2: Unequal list lengths</xml>
    val rq = range (parseQueries oqs) []
  in
    rands <- List.mapM (fn _ => rand) rq;
    rows <- List.mapM (fn r => world_find (clamp r)) rands;
    rands' <- List.mapM (fn _ => rand) rq;
    let
      val rows' = map2 (fn x y => x -- #RandomNumber ++ {RandomNumber = clamp y}) rows rands'
    in
      u <- List.mapM (fn r => dml (UPDATE world SET RandomNumber = {[r.RandomNumber]} WHERE Id = {[r.Id]})) rows';
      returnBlob (textBlob (toJson rows')) (blessMime "application/json")
    end
  end
