(** A record inclusion predicate *)

con incl :: K --> {K} -> {K} -> Type

val incl : K --> r1 :: {K} -> r2 :: {K} -> [r1 ~ r2] => incl r1 (r1 ++ r2)
val proj : r1 ::: {Type} -> r2 ::: {Type} -> incl r1 r2 -> $r2 -> $r1

val inv1 : K --> nm :: Name -> t :: K -> r :: {K} -> r' :: {K}
           -> [[nm] ~ r] =>
    f :: (Name -> K -> {K} -> Type)
    -> incl ([nm = t] ++ r) r'
    -> (nm :: Name -> t :: K -> r :: {K} -> [[nm] ~ r] => f nm t ([nm = t] ++ r))
    -> f nm t r'
val inv2 : K --> nm :: Name -> t :: K -> r :: {K} -> r' :: {K}
           -> [[nm] ~ r] =>
    incl ([nm = t] ++ r) r' -> incl r r'

val fold : K --> tf :: ({K} -> Type) -> r ::: {K}
           -> (nm :: Name -> v :: K -> r' :: {K}
               -> [[nm] ~ r'] => incl ([nm = v] ++ r') r -> tf r' -> tf ([nm = v] ++ r'))
           -> tf []
           -> folder r -> tf r
