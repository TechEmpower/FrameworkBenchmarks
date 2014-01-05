(** An Ur/Web solution to the TechEmpower web framework benchmarks.
  * For more information:
  * Benchmark specs: http://www.techempower.com/benchmarks/#section=code
  * Ur/Web: http://www.impredicative.com/ur/
  *)

open Json


(** * Utility functions *)

(** The spec requires including particular HTTP response headers that the
  * minimal Ur/Web HTTP servers don't return, so this function is responsible
  * for adding them. *)
fun addHeaders () =
  n <- now;
  setHeader (blessResponseHeader "Date") (timef "%a, %d %b %Y %H:%M:%S GMT" n);
  setHeader (blessResponseHeader "Server") "Ur/Web"

(** This function handles processing of a "queries" query string parameter to a
  * few of the benchmarks.  The URL representation is mandated by the spec, so
  * we don't get to use Ur/Web's standard handling of parameters.  Instead,
  * there's some manual parsing. :-( *)
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

(** Most of the benchmarks return results as JSON.
  * Here's a handy function to automate wrapping an arbitrary JSONable value as
  * a response. *)
fun returnJson [a] (_ : json a) (j : a) : transaction page =
  addHeaders ();
  returnBlob (textBlob (toJson j)) (blessMime "application/json")

(** Here's a similar, simpler function for returning plain text. *)
fun returnText (t : string) : transaction page =
  addHeaders ();
  returnBlob (textBlob t) (blessMime "text/plain")

(** Finally, an analogous wrapper for HTML *)
fun returnHtml (p : page) : transaction page =
  addHeaders ();
  return p


(** * Test type 1: JSON serialization *)

(** Let's teach the JSON library about a new record type. *)
type json_t = {Message : string}
val json_conversion : json json_t = json_record {Message = "message"}

fun json () =
    returnJson {Message = "Hello, World!"}


(** * Test type 2: Single database query *)

(** This test introduces a database table that a few other tests also use. *)
table world : {Id : int, RandomNumber : int} PRIMARY KEY Id

(** Let's tell the JSON library which record field names to use in
  * serialization. *)
type world_t = {Id : int, RandomNumber : int}
val world_conversion : json world_t =
    json_record {Id = "id", RandomNumber = "randomNumber"}

(** Helper function to look up the entry associated with an ID *)
fun world_find n =
  oneRow1 (SELECT World.Id, World.RandomNumber FROM world
           WHERE World.Id = {[n]})

(** In various tests, we'll be generating random IDs for this table.
  * Here's a quick way to do it, limiting the legal ID range. *)
val random_id =
  n <- rand;
  return ((n % 10000) + 1)

(** Finally, test 2 itself! *)
fun db () =
  n <- random_id;
  row <- world_find n;
  returnJson row


(** * Test type 3: Multiple database queries *)

fun queries oqs =
  rows <- List.tabulateM (fn _ => n <- random_id; world_find n) (parseQueries oqs);
  returnJson rows


(** * Test type 4: Fortunes *)

(** A new table, specific to this test *)
table fortune : {Id : int, Message : string} PRIMARY KEY Id

(** Teach the JSON library about good string names for the columns. *)
type fortune_t = {Id : int, Message : string}
val fortune_conversion : json fortune_t =
    json_record {Id = "id", Message = "message"}

(** Here's the additional fortune mandated by the spec. *)
val new_fortune : fortune_t =
    {Id = 0, Message = "Additional fortune added at request time"}

(** Actual page handler *)
fun fortunes () =
  fs <- queryL1 (SELECT Fortune.Id, Fortune.Message FROM fortune);
  fs' <- return (List.sort (fn x y => x.Message > y.Message)
                           (new_fortune :: fs));
  returnHtml <xml>
    <head><title>Fortunes</title></head>
    <body><table>
      <tr><th>id</th><th>message</th></tr>
      {List.mapX (fn f => <xml><tr>
        <td>{[f.Id]}</td><td>{[f.Message]}</td>
      </tr></xml>) fs'}
    </table></body>
  </xml>


(** * Test type 5: Database updates *)

fun updates oqs =
  rows <- List.tabulateM (fn _ => n <- random_id; world_find n)
                         (parseQueries oqs);
  rows' <- List.mapM (fn r => n <- random_id;
                         return (r -- #RandomNumber ++ {RandomNumber = n}))
                     rows;
  List.app (fn r => dml (UPDATE world SET RandomNumber = {[r.RandomNumber]}
                         WHERE Id = {[r.Id]})) rows';
  returnJson rows'


(** * Test type 6: Plaintext *)

fun plaintext () =
  returnText "Hello, World!"
