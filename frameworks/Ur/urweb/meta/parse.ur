(** Datatypes for describing parse results *)

datatype parse a =
         Success of a
       | Failure of string
