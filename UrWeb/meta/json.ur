con json a = {ToJson : a -> string,
              FromJson : string -> a * string}

fun mkJson [a] (x : {ToJson : a -> string,
                     FromJson : string -> a * string}) = x

fun skipSpaces s =
    let
        val len = String.length s

        fun skip i =
            if i >= len then
                ""
            else
                let
                    val ch = String.sub s i
                in
                    if Char.isSpace ch then
                        skip (i+1)
                    else
                        String.substring s {Start = i, Len = len-i}
                end
    in
        skip 0
    end

fun toJson [a] (j : json a) : a -> string = j.ToJson
fun fromJson' [a] (j : json a) : string -> a * string = j.FromJson

fun fromJson [a] (j : json a) (s : string) : a =
    let
        val (v, s') = j.FromJson (skipSpaces s)
    in
        if String.all Char.isSpace s' then
            v
        else
            error <xml>Extra content at end of JSON record: {[s']}</xml>
    end

fun escape s =
    let
        val len = String.length s

        fun esc i =
            if i >= len then
                "\""
            else
                let
                    val ch = String.sub s i
                in
                    (if ch = #"\"" || ch = #"\\" then
                         "\\" ^ String.str ch
                     else
                         String.str ch) ^ esc (i+1)
                end
    in
        "\"" ^ esc 0
    end

fun unescape s =
    let
        val len = String.length s

        fun findEnd i =
            if i >= len then
                error <xml>JSON unescape: string ends before quote: {[s]}</xml>
            else
                let
                    val ch = String.sub s i
                in
                    case ch of
                        #"\"" => i
                      | #"\\" =>
                        if i+1 >= len then
                            error <xml>JSON unescape: Bad escape sequence: {[s]}</xml>
                        else
                            findEnd (i+2)
                      | _ => findEnd (i+1)
                end

        val last = findEnd 1

        fun unesc i =
            if i >= last then
                ""
            else
                let
                    val ch = String.sub s i
                in
                    case ch of
                        #"\\" =>
                        if i+1 >= len then
                            error <xml>JSON unescape: Bad escape sequence: {[s]}</xml>
                        else
                            String.str (String.sub s (i+1)) ^ unesc (i+2)
                      | _ => String.str ch ^ unesc (i+1)
                end
    in
        if len = 0 || String.sub s 0 <> #"\"" then
            error <xml>JSON unescape: String doesn't start with double quote: {[s]}</xml>
        else
            (unesc 1, String.substring s {Start = last+1, Len = len-last-1})
    end

val json_string = {ToJson = escape,
                   FromJson = unescape}

fun numIn [a] (_ : read a) s : a * string =
    let
        val len = String.length s

        fun findEnd i =
            if i >= len then
                i
            else
                let
                    val ch = String.sub s i
                in
                    if Char.isDigit ch || ch = #"-" || ch = #"." || ch = #"E" || ch = #"e" then
                        findEnd (i+1)
                    else
                        i
                end

        val last = findEnd 0
    in
        (readError (String.substring s {Start = 0, Len = last}), String.substring s {Start = last, Len = len-last})
    end

fun json_num [a] (_ : show a) (_ : read a) : json a = {ToJson = show,
                                                       FromJson = numIn}

val json_int = json_num
val json_float = json_num

val json_bool = {ToJson = fn b => if b then "true" else "false",
                 FromJson = fn s => if String.isPrefix {Full = s, Prefix = "true"} then
                                        (True, String.substring s {Start = 4, Len = String.length s - 4})
                                    else if String.isPrefix {Full = s, Prefix = "false"} then
                                        (False, String.substring s {Start = 5, Len = String.length s - 5})
                                    else
                                        error <xml>JSON: bad boolean string: {[s]}</xml>}

fun json_option [a] (j : json a) : json (option a) =
    {ToJson = fn v => case v of
                          None => "null"
                        | Some v => j.ToJson v,
     FromJson = fn s => if String.isPrefix {Full = s, Prefix = "null"} then
                            (None, String.substring s {Start = 4, Len = String.length s - 4})
                        else
                            let
                                val (v, s') = j.FromJson s
                            in
                                (Some v, s')
                            end}

fun json_list [a] (j : json a) : json (list a) =
    let
        fun toJ' (ls : list a) : string =
            case ls of
                [] => ""
              | x :: ls => "," ^ toJson j x ^ toJ' ls

        fun toJ (x : list a) : string =
            case x of
                [] => "[]"
              | x :: [] => "[" ^ toJson j x ^ "]"
              | x :: ls => "[" ^ toJson j x ^ toJ' ls ^ "]"

        fun fromJ (s : string) : list a * string =
            let
                fun fromJ' (s : string) : list a * string =
                    if String.length s = 0 then
                        error <xml>JSON list doesn't end with ']'</xml>
                    else
                        let
                            val ch = String.sub s 0
                        in
                            case ch of
                                #"]" => ([], String.substring s {Start = 1, Len = String.length s - 1})
                              | _ =>
                                let
                                    val (x, s') = j.FromJson s
                                    val s' = skipSpaces s'
                                    val s' = if String.length s' = 0 then
                                                 error <xml>JSON list doesn't end with ']'</xml>
                                             else if String.sub s' 0 = #"," then
                                                 skipSpaces (String.substring s' {Start = 1, Len = String.length s' - 1})
                                             else
                                                 s'

                                    val (ls, s'') = fromJ' s'
                                in
                                    (x :: ls, s'')
                                end
                        end
            in
                if String.length s = 0 || String.sub s 0 <> #"[" then
                    error <xml>JSON list doesn't start with '[': {[s]}</xml>
                else
                    fromJ' (skipSpaces (String.substring s {Start = 1, Len = String.length s - 1}))
            end
    in
        {ToJson = toJ,
         FromJson = fromJ}
    end

fun json_record [ts ::: {Type}] (fl : folder ts) (jss : $(map json ts)) (names : $(map (fn _ => string) ts)) : json $ts =
    {ToJson = fn r => "{" ^ @foldR3 [json] [fn _ => string] [ident] [fn _ => string]
                             (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name v acc =>
                                 escape name ^ ":" ^ j.ToJson v ^ (case acc of
                                                                       "" => ""
                                                                     | _ => "," ^ acc))
                             "" fl jss names r ^ "}",
     FromJson = fn s =>
                   let
                       fun fromJ s (r : $(map option ts)) : $(map option ts) * string =
                           if String.length s = 0 then
                               error <xml>JSON object doesn't end in brace</xml>
                           else if String.sub s 0 = #"}" then
                               (r, String.substring s {Start = 1, Len = String.length s - 1})
                           else let
                                   val (name, s') = unescape s
                                   val s' = skipSpaces s'
                                   val s' = if String.length s' = 0 || String.sub s' 0 <> #":" then
                                                error <xml>No colon after JSON object field name</xml>
                                            else
                                                skipSpaces (String.substring s' {Start = 1, Len = String.length s' - 1})

                                   val (r, s') = @foldR2 [json] [fn _ => string] [fn ts => $(map option ts) -> $(map option ts) * string]
                                                  (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name' acc r =>
                                                      if name = name' then
                                                          let
                                                              val (v, s') = j.FromJson s'
                                                          in
                                                              (r -- nm ++ {nm = Some v}, s')
                                                          end
                                                      else
                                                          let
                                                              val (r', s') = acc (r -- nm)
                                                          in
                                                              (r' ++ {nm = r.nm}, s')
                                                          end)
                                                  (fn _ => error <xml>Unknown JSON object field name {[name]}</xml>)
                                                  fl jss names r

                                   val s' = skipSpaces s'
                                   val s' = if String.length s' <> 0 && String.sub s' 0 = #"," then
                                                skipSpaces (String.substring s' {Start = 1, Len = String.length s' - 1})
                                            else
                                                s'
                               in
                                   fromJ s' r
                               end
                   in
                       if String.length s = 0 || String.sub s 0 <> #"{" then
                           error <xml>JSON record doesn't begin with brace</xml>
                       else
                           let
                               val (r, s') = fromJ (skipSpaces (String.substring s {Start = 1, Len = String.length s - 1}))
                                                   (@map0 [option] (fn [t ::_] => None) fl)
                           in
                               (@map2 [option] [fn _ => string] [ident] (fn [t] (v : option t) name =>
                                                                            case v of
                                                                                None => error <xml>Missing JSON object field {[name]}</xml>
                                                                              | Some v => v) fl r names, s')
                           end
                   end}

fun json_variant [ts ::: {Type}] (fl : folder ts) (jss : $(map json ts)) (names : $(map (fn _ => string) ts)) : json (variant ts) =
    {ToJson = fn r => let val jnames = @map2 [json] [fn _ => string] [fn x => json x * string]
                                     (fn [t] (j : json t) (name : string) => (j, name)) fl jss names
                      in @Variant.destrR [ident] [fn x => json x * string]
                          (fn [p ::_] (v : p) (j : json p, name : string) =>
                            "{" ^ escape name ^ ":" ^ j.ToJson v ^ "}") fl r jnames
                      end,
     FromJson = fn s =>
                   if String.length s = 0 || String.sub s 0 <> #"{" then
                       error <xml>JSON variant doesn't begin with brace</xml>
                   else
                       let
                           val (name, s') = unescape (skipSpaces (String.suffix s 1))
                           val s' = skipSpaces s'
                           val s' = if String.length s' = 0 || String.sub s' 0 <> #":" then
                                        error <xml>No colon after JSON object field name</xml>
                                    else
                                        skipSpaces (String.substring s' {Start = 1, Len = String.length s' - 1})

                           val (r, s') = (@foldR2 [json] [fn _ => string]
                                            [fn ts => ts' :: {Type} -> [ts ~ ts'] => variant (ts ++ ts') * string]
                                            (fn [nm ::_] [t ::_] [rest ::_] [[nm] ~ rest] (j : json t) name'
                                             (acc : ts' :: {Type} -> [rest ~ ts'] => variant (rest ++ ts') * string) [fwd ::_] [[nm = t] ++ rest ~ fwd] =>
                                                if name = name'
                                                    then
                                                        let val (v, s') = j.FromJson s'
                                                        in (make [nm] v, s')
                                                        end
                                                    else acc [fwd ++ [nm = t]]
                                            )
                                            (fn [fwd ::_] [[] ~ fwd] => error <xml>Unknown JSON object variant name {[name]}</xml>)
                                            fl jss names) [[]] !

                           val s' = skipSpaces s'
                           val s' = if String.length s' <> 0 && String.sub s' 0 = #"," then
                                        skipSpaces (String.substring s' {Start = 1, Len = String.length s' - 1})
                                    else
                                        s'
                       in
                           if String.length s' = 0 then
                               error <xml>JSON object doesn't end in brace</xml>
                           else if String.sub s' 0 = #"}" then
                               (r, String.substring s' {Start = 1, Len = String.length s' - 1})
                           else error <xml>Junk after JSON value in object</xml>
                       end
                   }

val json_unit : json unit = json_record {} {}

functor Recursive (M : sig
                       con t :: Type -> Type
                       val json_t : a ::: Type -> json a -> json (t a)
                   end) = struct
    open M

    datatype r = Rec of t r

    fun rTo (Rec x) = (json_t {ToJson = rTo,
                               FromJson = fn _ => error <xml>Tried to FromJson in ToJson!</xml>}).ToJson x

    fun rFrom s =
        let
            val (x, s') = (json_t {ToJson = fn _ => error <xml>Tried to ToJson in FromJson!</xml>,
                                   FromJson = rFrom}).FromJson s
        in
            (Rec x, s')
        end

    val json_r = {ToJson = rTo, FromJson = rFrom}
end
