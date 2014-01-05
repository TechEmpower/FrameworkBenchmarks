(** A record membership predicate *)

con mem :: K --> Name -> K -> {K} -> Type

val mem : K --> nm :: Name -> t :: K -> r :: {K} -> [[nm] ~ r] => mem nm t ([nm = t] ++ r)
val mp : K --> K2 --> f :: (K -> K2) -> nm ::: Name -> t ::: K -> r ::: {K} -> mem nm t r -> mem nm (f t) (map f r)

val proj : nm ::: Name -> t ::: Type -> r ::: {Type} -> mem nm t r -> $r -> t
val replace : nm ::: Name -> t ::: Type -> r ::: {Type} -> mem nm t r -> $r -> t -> $r

val fold : K --> tf :: ({K} -> Type) -> r ::: {K}
           -> (nm :: Name -> v :: K -> r' :: {K} -> [[nm] ~ r']
               => mem nm v r -> tf r' -> tf ([nm = v] ++ r'))
           -> tf []
           -> folder r -> tf r
