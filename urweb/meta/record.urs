val numFields : r ::: {Type} -> folder r -> $r -> int

val mem : a ::: Type -> ns ::: {Unit} -> eq a -> folder ns -> a -> $(mapU a ns) -> bool
(* Is a value found in a record? *)

val equal : ts ::: {Type} -> $(map eq ts) -> folder ts -> $ts -> $ts -> bool
(* Are two records equal? *)

(* Analogous to applicative ap e.g. <*>, of type [f (a -> b) -> f a -> f b] *)
val ap : K --> tf1 :: (K -> Type) -> tf2 :: (K -> Type)
         -> r ::: {K} -> folder r
         -> $(map (fn t => tf1 t -> tf2 t) r)
         -> $(map tf1 r)
         -> $(map tf2 r)
