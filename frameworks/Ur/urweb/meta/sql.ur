fun sqexps [env] [fields] (fl : folder fields) (inj : $(map sql_injectable fields)) (r : $fields) =
    @map2 [sql_injectable] [ident] [sql_exp env [] []]
     (fn [t] => @sql_inject)
     fl inj r

fun selector [tn :: Name] [fs] [ofs] [fs ~ ofs] (fl : folder fs) (m : $(map sql_injectable fs)) (r : $fs)
    : sql_exp [tn = ofs ++ fs] [] [] bool =
    @foldR2 [sql_injectable] [ident]
     [fn key => rest :: {Type} -> [rest ~ key] => sql_exp [tn = key ++ rest] [] [] bool]
     (fn [nm :: Name] [t :: Type] [key :: {Type}] [[nm] ~ key]
                      (inj : sql_injectable t) (v : t)
                      (exp : rest :: {Type} -> [rest ~ key] => sql_exp [tn = key ++ rest] [] [] bool)
                      [rest :: {Type}] [rest ~ [nm = t] ++ key] =>
         (WHERE {{tn}}.{nm} = {@sql_inject inj v} AND {exp [[nm = t] ++ rest]}))
     (fn [rest :: {Type}] [rest ~ []] => (WHERE TRUE))
     fl m r [_] !

fun joiner [tn1 :: Name] [tn2 :: Name] [fs] [ofs1] [ofs2] [[tn1] ~ [tn2]] [fs ~ ofs1] [fs ~ ofs2]
           (fl : folder fs) : sql_exp [tn1 = ofs1 ++ fs, tn2 = ofs2 ++ fs] [] [] bool =
    @fold
     [fn key => rest1 :: {Type} -> rest2 :: {Type} -> [rest1 ~ key] => [rest2 ~ key] => sql_exp [tn1 = key ++ rest1, tn2 = key ++ rest2] [] [] bool]
     (fn [nm :: Name] [t :: Type] [key :: {Type}] [[nm] ~ key]
                      (exp : rest1 :: {Type} -> rest2 :: {Type} -> [rest1 ~ key] => [rest2 ~ key]
                       => sql_exp [tn1 = key ++ rest1, tn2 = key ++ rest2] [] [] bool)
                      [rest1 :: {Type}] [rest2 :: {Type}] [rest1 ~ [nm = t] ++ key] [rest2 ~ [nm = t] ++ key] =>
         (WHERE {{tn1}}.{nm} = {{tn2}}.{nm} AND {exp [[nm = t] ++ rest1] [[nm = t] ++ rest2]}))
     (fn [rest1 :: {Type}] [rest2 :: {Type}] [rest1 ~ []] [rest2 ~ []] => (WHERE TRUE))
     fl [_] [_] ! !

fun insertIfMissing [keyCols ::: {Type}] [otherCols ::: {Type}] [otherKeys ::: {{Unit}}]
                    [keyCols ~ otherCols] [[Pkey] ~ otherKeys]
                    (kfl : folder keyCols) (kinj : $(map sql_injectable keyCols))
                    (ofl : folder otherCols) (oinj : $(map sql_injectable otherCols))
                    (t : sql_table (keyCols ++ otherCols) ([Pkey = map (fn _ => ()) keyCols] ++ otherKeys))
                    (vs : $(keyCols ++ otherCols))
    : transaction bool =
    alreadyThere <- oneRowE1 (SELECT COUNT( * ) > 0
                              FROM t
                              WHERE {@selector [#T] ! kfl kinj (vs --- _)});
    if alreadyThere then
        return False
    else
        dml (insert t (@sqexps kfl kinj (vs --- _)
                        ++ @sqexps ofl oinj (vs --- _)));
        return True

fun deleteByKey [keyCols ::: {Type}] [otherCols ::: {Type}] [otherKeys ::: {{Unit}}]
    [keyCols ~ otherCols] [[Pkey] ~ otherKeys]
    (kfl : folder keyCols) (kinj : $(map sql_injectable keyCols))
    (t : sql_table (keyCols ++ otherCols) ([Pkey = map (fn _ => ()) keyCols] ++ otherKeys))
    (vs : $keyCols) =
    dml (delete t (@selector [#T] ! kfl kinj vs))

fun lookup [keyCols ::: {Type}] [otherCols ::: {Type}] [otherKeys ::: {{Unit}}]
           [keyCols ~ otherCols] [[Pkey] ~ otherKeys]
           (kfl : folder keyCols) (kinj : $(map sql_injectable keyCols))
           (t : sql_table (keyCols ++ otherCols) ([Pkey = map (fn _ => ()) keyCols] ++ otherKeys))
           (vs : $keyCols)
    : transaction (option $otherCols) =
      oneOrNoRows1 (SELECT t.{{otherCols}}
                    FROM t
                    WHERE {@selector [#T] ! kfl kinj (vs --- _)})

fun listify [lead :: Name] [cols ::: {Type}] [rest ::: {{Type}}] [[lead] ~ rest]
    (fl : folder cols) (eqs : $(map eq cols)) (q : sql_query [] [] ([lead = cols] ++ rest) []) =
    query q
    (fn r acc =>
        return (case acc of
                    [] => (r.lead, (r -- lead) :: []) :: []
                  | (key, ls) :: acc' =>
                    if @Record.equal eqs fl r.lead key then
                        (key, (r -- lead) :: ls) :: acc'
                    else
                        (r.lead, (r -- lead) :: []) :: acc))
    []
