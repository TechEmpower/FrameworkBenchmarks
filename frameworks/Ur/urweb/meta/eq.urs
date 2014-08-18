(** A constructor equality predicate *)

con eq :: K --> K -> K -> Type

val refl : K --> t ::: K -> eq t t
val sym : K --> t1 ::: K -> t2 ::: K -> eq t1 t2 -> eq t2 t1
val trans : K --> t1 ::: K -> t2 ::: K -> t3 ::: K -> eq t1 t2 -> eq t2 t3 -> eq t1 t3

val cast : K --> t1 ::: K -> t2 ::: K -> eq t1 t2 -> f :: (K -> Type) -> f t1 -> f t2

val fold : K --> tf :: ({K} -> Type) -> r ::: {K}
           -> (pre :: {K} -> nm :: Name -> v :: K -> post :: {K} -> [pre ~ post] => [[nm] ~ pre ++ post] =>
               eq r (pre ++ [nm = v] ++ post) -> tf post -> tf ([nm = v] ++ post))
           -> tf [] -> folder r -> tf r

val foldUR : tr :: Type -> tf :: ({Unit} -> Type) -> r ::: {Unit}
           -> (pre :: {Unit} -> nm :: Name -> post :: {Unit} -> [pre ~ post] => [[nm] ~ pre ++ post] =>
               eq r (pre ++ [nm] ++ post) -> tr -> tf post -> tf ([nm] ++ post))
           -> tf [] -> folder r -> $(mapU tr r) -> tf r

val foldR : K --> tr :: (K -> Type) -> tf :: ({K} -> Type) -> r ::: {K}
           -> (pre :: {K} -> nm :: Name -> t :: K -> post :: {K} -> [pre ~ post] => [[nm] ~ pre ++ post] =>
               eq r (pre ++ [nm = t] ++ post) -> tr t -> tf post -> tf ([nm = t] ++ post))
           -> tf [] -> folder r -> $(map tr r) -> tf r

val foldR2 : K --> tr1 :: (K -> Type) -> tr2 :: (K -> Type) -> tf :: ({K} -> Type) -> r ::: {K}
             -> (pre :: {K} -> nm :: Name -> t :: K -> post :: {K} -> [pre ~ post] => [[nm] ~ pre ++ post] =>
                 eq r (pre ++ [nm = t] ++ post) -> tr1 t -> tr2 t -> tf post -> tf ([nm = t] ++ post))
             -> tf [] -> folder r -> $(map tr1 r) -> $(map tr2 r) -> tf r

val foldR3 : K --> tr1 :: (K -> Type) -> tr2 :: (K -> Type) -> tr3 :: (K -> Type) -> tf :: ({K} -> Type) -> r ::: {K}
             -> (pre :: {K} -> nm :: Name -> t :: K -> post :: {K} -> [pre ~ post] => [[nm] ~ pre ++ post] =>
                 eq r (pre ++ [nm = t] ++ post) -> tr1 t -> tr2 t -> tr3 t -> tf post -> tf ([nm = t] ++ post))
             -> tf [] -> folder r -> $(map tr1 r) -> $(map tr2 r) -> $(map tr3 r) -> tf r

val foldR4 : K --> tr1 :: (K -> Type) -> tr2 :: (K -> Type) -> tr3 :: (K -> Type) -> tr4 :: (K -> Type) -> tf :: ({K} -> Type) -> r ::: {K}
             -> (pre :: {K} -> nm :: Name -> t :: K -> post :: {K} -> [pre ~ post] => [[nm] ~ pre ++ post] =>
                 eq r (pre ++ [nm = t] ++ post) -> tr1 t -> tr2 t -> tr3 t -> tr4 t -> tf post -> tf ([nm = t] ++ post))
             -> tf [] -> folder r -> $(map tr1 r) -> $(map tr2 r) -> $(map tr3 r) -> $(map tr4 r) -> tf r

val mp : K --> tr :: (K -> Type) -> tf :: (K -> Type) -> r ::: {K}
         -> (nm :: Name -> t :: K -> rest :: {K} -> [[nm] ~ rest] =>
             eq r ([nm = t] ++ rest) -> tr t -> tf t)
         -> folder r -> $(map tr r) -> $(map tf r)
