(** The JSON text-based serialization format *)

class json

val toJson : a ::: Type -> json a -> a -> string
val fromJson : a ::: Type -> json a -> string -> a
val fromJson' : a ::: Type -> json a -> string -> a * string

val mkJson : a ::: Type -> {ToJson : a -> string,
                            FromJson : string -> a * string} -> json a

val json_string : json string
val json_int : json int
val json_float : json float
val json_bool : json bool
val json_option : a ::: Type -> json a -> json (option a)
val json_list : a ::: Type -> json a -> json (list a)

val json_record : ts ::: {Type} -> folder ts -> $(map json ts) -> $(map (fn _ => string) ts) -> json $ts
val json_variant : ts ::: {Type} -> folder ts -> $(map json ts) -> $(map (fn _ => string) ts) -> json (variant ts)

val json_unit : json unit

functor Recursive (M : sig
                       con t :: Type -> Type
                       val json_t : a ::: Type -> json a -> json (t a)
                   end) : sig
    datatype r = Rec of M.t r

    val json_r : json r
end
