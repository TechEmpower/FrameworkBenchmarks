con eq = K ==> fn (t1 :: K) (t2 :: K) => f :: (K -> Type) -> f t1 -> f t2

val refl [K] [t ::: K] : eq t t = fn [f :: (K -> Type)] x => x

fun sym [K] [t1 ::: K] [t2 ::: K] (e : eq t1 t2) : eq t2 t1 =
    e [fn t => eq t t1] refl

fun trans [K] [t1 ::: K] [t2 ::: K] [t3 ::: K] (e1 : eq t1 t2) (e2 : eq t2 t3) : eq t1 t3 =
    (sym e1) [fn t => eq t t3] e2

fun cast [K] [t1 ::: K] [t2 ::: K] (e : eq t1 t2) = e

fun fold [K] [tf :: {K} -> Type] [r ::: {K}]
         (f : pre :: {K} -> nm :: Name -> v :: K -> post :: {K} -> [pre ~ post] => [[nm] ~ pre ++ post] =>
          eq r (pre ++ [nm = v] ++ post) -> tf post -> tf ([nm = v] ++ post))
    (i : tf []) (fl : folder r) : tf r =
    @@Top.fold [fn post => pre :: {K} -> [pre ~ post] => eq r (pre ++ post) -> tf post]
     (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest]
                      (acc : pre :: {K} -> [pre ~ rest] => eq r (pre ++ rest) -> tf rest)
                      [pre :: {K}] [pre ~ [nm = t] ++ rest] pf =>
         f [pre] [nm] [t] [rest] pf (acc [[nm = t] ++ pre] pf))
     (fn [pre :: {K}] [pre ~ []] _ => i) [r] fl [[]] ! refl

fun foldUR [tr :: Type] [tf :: {Unit} -> Type] [r ::: {Unit}]
    (f : pre :: {Unit} -> nm :: Name -> post :: {Unit} -> [pre ~ post] => [[nm] ~ pre ++ post] =>
     eq r (pre ++ [nm] ++ post) -> tr -> tf post -> tf ([nm] ++ post))
    (i : tf []) (fl : folder r) (r : $(mapU tr r)) : tf r =
    @@fold [fn r' => $(mapU tr r') -> tf r'] [r]
      (fn [pre :: {Unit}] [nm :: Name] [u :: Unit] [post :: {Unit}] [pre ~ post] [[nm] ~ pre ++ post] pf acc r =>
          f [pre] [nm] [post] pf r.nm (acc (r -- nm)))
      (fn _ => i) fl r

fun foldR [K] [tr :: K -> Type] [tf :: {K} -> Type] [r ::: {K}]
    (f : pre :: {K} -> nm :: Name -> t :: K -> post :: {K} -> [pre ~ post] => [[nm] ~ pre ++ post] =>
     eq r (pre ++ [nm = t] ++ post) -> tr t -> tf post -> tf ([nm = t] ++ post))
    (i : tf []) (fl : folder r) (r : $(map tr r)) : tf r =
    @@fold [fn r' => $(map tr r') -> tf r'] [r]
      (fn [pre :: {K}] [nm :: Name] [t :: K] [post :: {K}] [pre ~ post] [[nm] ~ pre ++ post] pf acc r =>
          f [pre] [nm] [t] [post] pf r.nm (acc (r -- nm)))
      (fn _ => i) fl r

fun foldR2 [K] [tr1 :: K -> Type] [tr2 :: K -> Type] [tf :: {K} -> Type] [r ::: {K}]
    (f : pre :: {K} -> nm :: Name -> t :: K -> post :: {K} -> [pre ~ post] => [[nm] ~ pre ++ post] =>
     eq r (pre ++ [nm = t] ++ post) -> tr1 t -> tr2 t -> tf post -> tf ([nm = t] ++ post))
    (i : tf []) (fl : folder r) (r1 : $(map tr1 r)) (r2 : $(map tr2 r)) : tf r =
    @@fold [fn r' => $(map tr1 r') -> $(map tr2 r') -> tf r'] [r]
      (fn [pre :: {K}] [nm :: Name] [t :: K] [post :: {K}] [pre ~ post] [[nm] ~ pre ++ post] pf acc r1 r2 =>
          f [pre] [nm] [t] [post] pf r1.nm r2.nm (acc (r1 -- nm) (r2 -- nm)))
      (fn _ _ => i) fl r1 r2

fun foldR3 [K] [tr1 :: K -> Type] [tr2 :: K -> Type] [tr3 :: K -> Type] [tf :: {K} -> Type] [r ::: {K}]
    (f : pre :: {K} -> nm :: Name -> t :: K -> post :: {K} -> [pre ~ post] => [[nm] ~ pre ++ post] =>
     eq r (pre ++ [nm = t] ++ post) -> tr1 t -> tr2 t -> tr3 t -> tf post -> tf ([nm = t] ++ post))
    (i : tf []) (fl : folder r) (r1 : $(map tr1 r)) (r2 : $(map tr2 r)) (r3 : $(map tr3 r)) : tf r =
    @@fold [fn r' => $(map tr1 r') -> $(map tr2 r') -> $(map tr3 r') -> tf r'] [r]
      (fn [pre :: {K}] [nm :: Name] [t :: K] [post :: {K}] [pre ~ post] [[nm] ~ pre ++ post] pf acc r1 r2 r3 =>
          f [pre] [nm] [t] [post] pf r1.nm r2.nm r3.nm (acc (r1 -- nm) (r2 -- nm) (r3 -- nm)))
      (fn _ _ _ => i) fl r1 r2 r3

fun foldR4 [K] [tr1 :: K -> Type] [tr2 :: K -> Type] [tr3 :: K -> Type] [tr4 :: K -> Type] [tf :: {K} -> Type] [r ::: {K}]
    (f : pre :: {K} -> nm :: Name -> t :: K -> post :: {K} -> [pre ~ post] => [[nm] ~ pre ++ post] =>
     eq r (pre ++ [nm = t] ++ post) -> tr1 t -> tr2 t -> tr3 t -> tr4 t -> tf post -> tf ([nm = t] ++ post))
    (i : tf []) (fl : folder r) (r1 : $(map tr1 r)) (r2 : $(map tr2 r)) (r3 : $(map tr3 r)) (r4 : $(map tr4 r)) : tf r =
    @@fold [fn r' => $(map tr1 r') -> $(map tr2 r') -> $(map tr3 r') -> $(map tr4 r') -> tf r'] [r]
      (fn [pre :: {K}] [nm :: Name] [t :: K] [post :: {K}] [pre ~ post] [[nm] ~ pre ++ post] pf acc r1 r2 r3 r4 =>
          f [pre] [nm] [t] [post] pf r1.nm r2.nm r3.nm r4.nm (acc (r1 -- nm) (r2 -- nm) (r3 -- nm) (r4 -- nm)))
      (fn _ _ _ _ => i) fl r1 r2 r3 r4

fun mp [K] [tr :: K -> Type] [tf :: K -> Type] [r ::: {K}]
       (f : nm :: Name -> t :: K -> rest :: {K} -> [[nm] ~ rest] =>
        eq r ([nm = t] ++ rest) -> tr t -> tf t)
    (fl : folder r) (r : $(map tr r)) : $(map tf r) =
  @@foldR [tr] [fn r => $(map tf r)] [r]
      (fn [pre :: {K}] [nm :: Name] [t :: K] [post :: {K}] [pre ~ post] [[nm] ~ pre ++ post] pf r acc =>
          {nm = f [nm] [t] [pre ++ post] pf r} ++ acc)
      {} fl r
