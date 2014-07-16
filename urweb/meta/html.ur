open Parse

con attribute = fn t => {Nam : string,
                         Parse : string -> option t}

con tag = fn ts => {Nam : string,
                    Attributes : $(map attribute ts),
                    Folder : folder ts,
                    Construct : ctx ::: {Unit} -> [[Body] ~ ctx] => $ts
                                -> xml ([Body] ++ ctx) [] [] -> xml ([Body] ++ ctx) [] []}

fun tag [use] [ignore] [use ~ ignore] (fl : folder use) (name : string) (attrs : $(map attribute use))
        (construct : ctx ::: {Unit} -> [[Body] ~ ctx] => Basis.tag (use ++ ignore) ([Body] ++ ctx) ([Body] ++ ctx) [] []) =
    {Nam = name,
     Attributes = attrs,
     Folder = fl,
     Construct = fn [ctx] [[Body] ~ ctx] (ats : $use) (inner : xml ([Body] ++ ctx) [] []) =>
                    Basis.tag null None noStyle None ats construct inner}

fun simpleTag [ignore] name (bt : bodyTag ignore) : tag [] =
    @@tag [[]] [ignore] ! _ name {} (fn [ctx] [[Body] ~ ctx] => bt ())

fun simpleTag' [use] [ignore] [use ~ ignore] (fl : folder use)
               name (bt : bodyTag (use ++ ignore)) (ats : $(map attribute use)) : tag use =
    @@tag [use] [ignore] ! fl name ats (fn [ctx] [[Body] ~ ctx] => bt ())

fun url name = {Nam = name,
                Parse = checkUrl}

datatype error a =
         Good of a
       | Bad of string

fun format [tags] (fl : folder tags) (tags : $(map tag tags)) [ctx] [[Body] ~ ctx] s =
    let
        fun loop s : error (xml ([Body] ++ ctx) [] [] * string) =
            case String.msplit {Haystack = s, Needle = "&<"} of
                None => Good (cdata s, "")
              | Some (pre, ch, post) =>
                case ch of
                    #"&" =>
                    (case String.split post #";" of
                         None => Bad "No ';' after '&'"
                       | Some (code, post) =>
                         let
                             val xml = 
                                 case code of
                                     "lt" => <xml>&lt;</xml>
                                   | "gt" => <xml>&gt;</xml>
                                   | "amp" => <xml>&amp;</xml>
                                   | _ => <xml/>
                         in
                             case loop post of
                                 Good (after, post) => Good (<xml>{[pre]}{xml}{after}</xml>, post)
                               | x => x
                         end)
                  | _ =>
                    if String.length post > 0 && String.sub post 0 = #"/" then
                        case String.split post #"\x3E" of
                            None => Bad "No '>' after '</'"
                          | Some (_, post) => Good (<xml>{[pre]}</xml>, post)
                    else
                        case String.msplit {Haystack = post, Needle = " >"} of
                            None => Bad "No '>' after '<'"
                          | Some (tname, ch, post) =>
                            @foldR [tag] [fn _ => unit -> error (xml ([Body] ++ ctx) [] [] * string)]
                            (fn [nm :: Name] [ts :: {Type}] [r :: {{Type}}] [[nm] ~ r] (meta : tag ts) acc () =>
                                if meta.Nam = tname then
                                    let
                                        fun doAttrs (ch, post, ats : $(map option ts)) =
                                            if String.length post > 0 && Char.isSpace (String.sub post 0) then
                                                doAttrs (ch, String.substring post {Start = 1,
                                                                                    Len = String.length post - 1},
                                                         ats)
                                            else 
                                                case ch of
                                                    #"\x3E" => Good (ats, post)
                                                  | _ =>
                                                    case String.split post #"=" of
                                                        None =>
                                                        (case String.split post #"\x3E" of
                                                             None => Bad "No tag ender '\x3E'"
                                                           | Some (_, post) => Good (ats, post))
                                                      | Some (aname, post) =>
                                                        if String.length post >= 1 && String.sub post 0 = #"\"" then
                                                            case String.split (String.substring post
                                                                                                {Start = 1,
                                                                                                 Len = String.length post
                                                                                                       - 1})
                                                                              #"\"" of
                                                                None => Bad "No '\"' to end attribute value"
                                                              | Some (aval, post) =>
                                                                let
                                                                    val ats =
                                                                        @map2 [attribute] [option] [option]
                                                                         (fn [t] meta v =>
                                                                             if aname = meta.Nam then
                                                                                 meta.Parse aval
                                                                             else
                                                                                 v)
                                                                         meta.Folder meta.Attributes ats
                                                                in
                                                                    doAttrs (#" ", post, ats)
                                                                end
                                                        else
                                                            Bad "Attribute value doesn't begin with quote"
                                    in
                                        case doAttrs (ch, post, @map0 [option] (fn [t :: Type] => None)
                                                                 meta.Folder) of
                                            Good (ats, post) =>
                                            let
                                                val ats =
                                                    @map2 [attribute] [option] [ident]
                                                     (fn [t] meta v =>
                                                         case v of
                                                             None => error <xml>Missing attribute {[meta.Nam]}
                                                               for {[tname]}</xml>
                                                           | Some v => v)
                                                     meta.Folder meta.Attributes ats
                                            in
                                                case loop post of
                                                    Good (inner, post) =>
                                                    (case loop post of
                                                         Good (after, post) =>
                                                         Good (<xml>{[pre]}{meta.Construct [ctx] !
                                                                                           ats inner}{after}</xml>, post)
                                                       | x => x)
                                                  | x => x
                                            end
                                          | Bad s => Bad s
                                    end
                                else
                                    acc ())
                            (fn () => Bad ("Unknown HTML tag " ^ tname)) fl tags ()
    in
        case loop s of
            Bad msg => Failure msg
          | Good (xml, _) => Success xml
    end

val b = simpleTag "b" @@b
val i = simpleTag "i" @@i
val a = simpleTag' "a" @@a {Href = url "href"}

