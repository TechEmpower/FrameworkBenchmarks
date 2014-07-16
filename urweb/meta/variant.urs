(** Derived functions dealing with polymorphic variants *)

val read : r ::: {Unit} -> t ::: Type -> folder r -> $(mapU t r) -> variant (mapU {} r) -> t
val write : r ::: {Unit} -> t ::: Type -> folder r -> $(mapU t r) -> variant (mapU {} r) -> t -> $(mapU t r)

val search : r ::: {Unit} -> t ::: Type -> (variant (mapU {} r) -> option t) -> folder r -> option t
val find : r ::: {Unit} -> (variant (mapU {} r) -> bool) -> folder r -> option (variant (mapU {} r))

val test : nm :: Name -> t ::: Type -> ts ::: {Type} -> [[nm] ~ ts] => folder ([nm = t] ++ ts)
                                                                    -> variant ([nm = t] ++ ts) -> option t
val testLess : nm :: Name -> t ::: Type -> ts ::: {Type} -> [[nm] ~ ts] => folder ts -> variant ([nm = t] ++ ts) -> option t

val weaken : r1 ::: {Type} -> r2 ::: {Type} -> [r1 ~ r2] => folder r1
             -> variant r1 -> variant (r1 ++ r2)

val testEq : K --> f :: (K -> Type) -> nm :: Name -> t ::: K -> ts ::: {K} -> r ::: {K} -> [[nm] ~ ts] =>
    Eq.eq r ([nm = t] ++ ts)
    -> folder r
    -> variant (map f r) -> option (f t)

val eq : r ::: {Unit} -> folder r -> variant (mapU {} r) -> variant (mapU {} r) -> bool

val makeEq : K --> f :: (K -> Type) -> nm :: Name -> t ::: K -> ts ::: {K} -> r ::: {K} -> [[nm] ~ ts] =>
    Eq.eq r ([nm = t] ++ ts)
    -> f t
    -> variant (map f r)

val mp : r ::: {Unit} -> t ::: Type -> folder r -> (variant (mapU {} r) -> t) -> $(mapU t r)

val fold : r ::: {Unit} -> t ::: Type -> folder r -> (variant (mapU {} r) -> t -> t) -> t -> t

val foldR : tr ::: Type -> r ::: {Unit} -> t ::: Type -> folder r -> (variant (mapU {} r) -> tr -> t -> t) -> $(mapU tr r) -> t -> t

val appR : m ::: (Type -> Type) -> monad m
           -> tr ::: Type -> r ::: {Unit} -> folder r -> (variant (mapU {} r) -> tr -> m {}) -> $(mapU tr r) -> m {}

val mapR : tr ::: Type -> t ::: Type -> r ::: {Unit} -> folder r -> (variant (mapU {} r) -> tr -> t) -> $(mapU tr r) -> $(mapU t r)

val destrR : K --> f :: (K -> Type) -> fr :: (K -> Type) -> t ::: Type
             -> (p :: K -> f p -> fr p -> t)
             -> r ::: {K} -> folder r -> variant (map f r) -> $(map fr r) -> t

val destr2R : K --> f1 :: (K -> Type) -> f2 :: (K -> Type) -> fr :: (K -> Type) -> t ::: Type
             -> (p :: K -> f1 p -> f2 p -> fr p -> t)
             -> r ::: {K} -> folder r -> variant (map f1 r) -> variant (map f2 r) -> $(map fr r) -> option t

(* Metaprogrammed type-directed case-match.

This uses a combination of type classes and metaprogramming to make
it easy to write case-matches on very large variants with many
similar elements.  Here's how you use it:

    1. For every type in the variant, write a local typeclass function
       which reduces it to t, and register as such using the 'declareCase'
       function in the module you created.

            let val empty = declareCase (fn _ (_ : int) => True)

       These functions also take an initial argument, which has
       type [a -> variant ts]; e.g. you can use this to create
       a new copy of the variant with different values!
       Make sure you specify type signatures on the argument [t]
       so that we can identify who this typeclass is for.  (If you
       use type classes to construct the return value, you may
       also need to declare the return type explicitly.)

    2. Do the match using 'typeCase':

            typeCase t

       If you need to override specific constructors, use this idiom:

            @typeCase t (_ ++ {
                YourConstr = declareCase (fn _ _ => ...)
            }) _

How does it work?  Very simple: it uses local typeclasses + Ur/Web's
support for automatically instantiating records of typeclasses.
*)

class type_case :: {Type} -> Type -> Type -> Type
val declareCase : ts ::: {Type} -> t ::: Type -> a ::: Type -> ((a -> variant ts) -> a -> t) -> type_case ts t a
val typeCase : ts ::: {Type} -> t ::: Type -> variant ts -> $(map (type_case ts t) ts) -> folder ts -> t
