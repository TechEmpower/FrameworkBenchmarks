(** Common metaprogramming patterns for SQL syntax construction *)

val sqexps : env ::: {{Type}} -> fields ::: {Type} -> folder fields -> $(map sql_injectable fields)
             -> $fields -> $(map (sql_exp env [] []) fields)
(* Convert a record of Ur values into a record of SQL expressions *)

val selector : tn :: Name -> fs ::: {Type} -> ofs ::: {Type} -> [fs ~ ofs]
               => folder fs -> $(map sql_injectable fs) -> $fs
               -> sql_exp [tn = ofs ++ fs] [] [] bool
(* Build a boolean SQL expression expressing equality of some fields of a table
 * row with a record of Ur values *)

val joiner : tn1 :: Name -> tn2 :: Name -> fs ::: {Type} -> ofs1 ::: {Type} -> ofs2 ::: {Type}
             -> [[tn1] ~ [tn2]] => [fs ~ ofs1] => [fs ~ ofs2]
               => folder fs
               -> sql_exp [tn1 = ofs1 ++ fs, tn2 = ofs2 ++ fs] [] [] bool
(* Declare equality of same-named columns from two tables. *)

val insertIfMissing : keyCols ::: {Type} -> otherCols ::: {Type} -> otherKeys ::: {{Unit}}
                      -> [keyCols ~ otherCols] => [[Pkey] ~ otherKeys]
                      => folder keyCols -> $(map sql_injectable keyCols)
                      -> folder otherCols -> $(map sql_injectable otherCols)
                      -> sql_table (keyCols ++ otherCols) ([Pkey = map (fn _ => ()) keyCols] ++ otherKeys)
                      -> $(keyCols ++ otherCols)
                      -> transaction bool
(* Insert a row into an SQL table if its key isn't already present, returning [False] iff the key was already present *)

val deleteByKey : keyCols ::: {Type} -> otherCols ::: {Type} -> otherKeys ::: {{Unit}}
                  -> [keyCols ~ otherCols] => [[Pkey] ~ otherKeys]
                  => folder keyCols -> $(map sql_injectable keyCols)
                  -> sql_table (keyCols ++ otherCols) ([Pkey = map (fn _ => ()) keyCols] ++ otherKeys)
                  -> $keyCols
                  -> transaction {}
(* Delete a row from a table by matching its primary key against a given record. *)

val lookup : keyCols ::: {Type} -> otherCols ::: {Type} -> otherKeys ::: {{Unit}}
             -> [keyCols ~ otherCols] => [[Pkey] ~ otherKeys]
             => folder keyCols -> $(map sql_injectable keyCols)
             -> sql_table (keyCols ++ otherCols) ([Pkey = map (fn _ => ()) keyCols] ++ otherKeys)
             -> $keyCols -> transaction (option $otherCols)
(* Get the further columns associated with a table key. *)

val listify : lead :: Name -> cols ::: {Type} -> rest ::: {{Type}} -> [[lead] ~ rest]
              => folder cols -> $(map eq cols)
              -> sql_query [] [] ([lead = cols] ++ rest) []
              -> transaction (list ($cols * list $(map (fn ts => $ts) rest)))
(* Shrink a set of table rows by summarizing into lists, keyed off of a lead table *)
