fun read [r ::: {Unit}] [t ::: Type] (fl : folder r) (r : $(mapU t r)) (v : variant (mapU {} r)) : t =
    match v
    (@fold [fn r => r' :: {Unit} -> [r ~ r'] => $(mapU t (r ++ r')) -> $(mapU ({} -> t) r)]
     (fn [nm :: Name] [u::_] [us::_] [[nm] ~ us] (cs : r' :: {Unit} -> [us ~ r'] => $(mapU t (us ++ r')) -> _) [r'::_] [[nm = u] ++ us ~ r'] r =>
         {nm = fn () => r.nm} ++ cs [[nm = u] ++ r'] r)
     (fn [r'::_] [[] ~ r'] _ => {}) fl [[]] ! r)

fun write [r ::: {Unit}] [t ::: Type] (fl : folder r) (r : $(mapU t r)) (v : variant (mapU {} r)) (x : t) : $(mapU t r) =
    match v
    (@fold [fn r => r' :: {Unit} -> [r ~ r'] => $(mapU t (r ++ r')) -> $(mapU ({} -> $(mapU t (r ++ r'))) r)]
      (fn [nm :: Name] [u::_] [us::_] [[nm] ~ us]
          (cs : r' :: {Unit} -> [us ~ r'] => $(mapU t (us ++ r')) -> $(mapU ({} -> $(mapU t (us ++ r'))) us))
          [r'::_] [[nm = u] ++ us ~ r'] r =>
          {nm = fn () => r -- nm ++ {nm = x}} ++ cs [[nm = u] ++ r'] r)
      (fn [r'::_] [[] ~ r'] _ => {}) fl [[]] ! r)

fun search [r] [t] (f : variant (mapU {} r) -> option t) (fl : folder r) : option t =
    @fold [fn r => r' :: {Unit} -> [r ~ r'] => (variant (mapU {} (r ++ r')) -> option t) -> option t]
    (fn [nm :: Name] [u ::_] [r ::_] [[nm] ~ r]
                     (acc : r' :: {Unit} -> [r ~ r'] => (variant (mapU {} (r ++ r')) -> option t) -> option t)
                     [r' ::_] [[nm] ++ r ~ r'] f' =>
        case f' (make [nm] {}) of
            None => acc [[nm] ++ r'] f'
          | v => v)
    (fn [r' ::_] [[] ~ r'] _ => None) fl [[]] ! f

fun find [r] (f : variant (mapU {} r) -> bool) (fl : folder r) : option (variant (mapU {} r)) =
    @search (fn v => if f v then Some v else None) fl

fun test [nm :: Name] [t ::: Type] [ts ::: {Type}] [[nm] ~ ts] (fl : folder ([nm = t] ++ ts))
          (v : variant ([nm = t] ++ ts)) : option t =
    match v ({nm = Some}
                 ++ (@map0 [fn t' => t' -> option t] (fn [t' :: Type] _ => None) fl -- nm))

fun testLess [nm :: Name] [t ::: Type] [ts ::: {Type}] [[nm] ~ ts] (fl : folder ts) (v : variant ([nm = t] ++ ts)) : option t =
    match v ({nm = Some}
                 ++ @map0 [fn t' => t' -> option t] (fn [t' :: Type] _ => None) fl)

fun weaken [r1 ::: {Type}] [r2 ::: {Type}] [r1 ~ r2] (fl : folder r1) (v : variant r1) : variant (r1 ++ r2) =
    match v
    (@fold [fn r => r' :: {Type} -> [r ~ r'] => $(map (fn t => t -> variant (r ++ r')) r)]
      (fn [nm :: Name] [t ::_] [r ::_] [[nm] ~ r] (acc : r' :: {Type} -> [r ~ r'] => $(map (fn t => t -> variant (r ++ r')) r)) [r'::_] [[nm = t] ++ r ~ r'] =>
          {nm = make [nm]} ++ acc [[nm = t] ++ r'])
      (fn [r'::_] [[] ~ r'] => {}) fl [r2] !)

fun eq [r] (fl : folder r) (v1 : variant (mapU {} r)) (v2 : variant (mapU {} r)) : bool =
    match v1
    (@fold [fn r => r' :: {Unit} -> [r ~ r'] => folder (r ++ r') -> variant (mapU {} (r ++ r')) -> $(mapU ({} -> bool) r)]
     (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r]
         (acc : r' :: {Unit} -> [r ~ r'] => folder (r ++ r') -> variant (mapU {} (r ++ r')) -> $(mapU ({} -> bool) r))
         [r' ::_] [[nm] ++ r ~ r'] (fl' : folder ([nm] ++ r ++ r')) v =>
         {nm = fn () => Option.isSome (@test [nm] ! (@Folder.mp fl') v)}
             ++ @acc [[nm] ++ r'] ! fl' v)
     (fn [r' ::_] [[] ~ r'] _ _ => {}) fl [[]] ! fl v2)

fun fold [r] [t] (fl : folder r) (f : variant (mapU {} r) -> t -> t) : t -> t =
    @Top.fold [fn r => r' :: {Unit} -> [r ~ r'] => (variant (mapU {} (r ++ r')) -> t -> t) -> t -> t]
    (fn [nm :: Name] [u ::_] [r ::_] [[nm] ~ r]
                     (acc : r' :: {Unit} -> [r ~ r'] => (variant (mapU {} (r ++ r')) -> t -> t) -> t -> t)
                     [r' ::_] [[nm] ++ r ~ r'] f' acc' =>
        f' (make [nm] {}) (acc [[nm] ++ r'] f' acc'))
    (fn [r' ::_] [[] ~ r'] _ x => x) fl [[]] ! f

fun mp [r ::: {Unit}] [t ::: Type] (fl : folder r) (f : variant (mapU {} r) -> t) : $(mapU t r) =
    @Top.fold [fn r => r' :: {Unit} -> [r ~ r'] => (variant (mapU {} (r ++ r')) -> t) -> $(mapU t r)]
    (fn [nm :: Name] [u ::_] [r ::_] [[nm] ~ r]
                     (acc : r' :: {Unit} -> [r ~ r'] => (variant (mapU {} (r ++ r')) -> t) -> $(mapU t r))
                     [r' ::_] [[nm] ++ r ~ r'] f' =>
        {nm = f' (make [nm] {})} ++ acc [[nm] ++ r'] f')
    (fn [r' ::_] [[] ~ r'] _ => {}) fl [[]] ! f

fun foldR [tr] [r] [t] (fl : folder r) (f : variant (mapU {} r) -> tr -> t -> t) (record : $(mapU tr r)) : t -> t =
    @Top.foldUR [tr] [fn r => r' :: {Unit} -> [r ~ r'] => (variant (mapU {} (r ++ r')) -> tr -> t -> t) -> t -> t]
    (fn [nm :: Name] [r ::_] [[nm] ~ r] (v : tr)
                     (acc : r' :: {Unit} -> [r ~ r'] => (variant (mapU {} (r ++ r')) -> tr -> t -> t) -> t -> t)
                     [r' ::_] [[nm] ++ r ~ r'] f' acc' =>
        f' (make [nm] {}) v (acc [[nm] ++ r'] f' acc'))
    (fn [r' ::_] [[] ~ r'] _ x => x) fl record [[]] ! f

fun appR [m] (_ : monad m) [tr] [r] (fl : folder r) (f : variant (mapU {} r) -> tr -> m {}) (record : $(mapU tr r)) : m {} =
    @foldR fl (fn var this acc => f var this; acc) record (return ())

fun mapR [tr] [t] [r] (fl : folder r) (f : variant (mapU {} r) -> tr -> t) (record : $(mapU tr r)) : $(mapU t r) =
    @Top.fold [fn r => r' :: {Unit} -> [r ~ r'] => (variant (mapU {} (r ++ r')) -> tr -> t) -> $(mapU tr r) -> $(mapU t r)]
    (fn [nm :: Name] [u ::_] [r ::_] [[nm] ~ r]
                     (acc : r' :: {Unit} -> [r ~ r'] => (variant (mapU {} (r ++ r')) -> tr -> t) -> $(mapU tr r) -> $(mapU t r))
                     [r' ::_] [[nm] ++ r ~ r'] f' vs =>
        {nm = f' (make [nm] {}) vs.nm} ++ acc [[nm] ++ r'] f' (vs -- nm))
    (fn [r' ::_] [[] ~ r'] _ _ => {}) fl [[]] ! f record

fun destrR [K] [f :: K -> Type] [fr :: K -> Type] [t ::: Type]
    (f : p :: K -> f p -> fr p -> t)
    [r ::: {K}] (fl : folder r) (v : variant (map f r)) (r : $(map fr r)) : t =
    match v
    (@Top.mp [fr] [fn p => f p -> t]
     (fn [p] (m : fr p) (v : f p) => f [p] v m)
     fl r)

fun destr2R [K] [f1 :: K -> Type] [f2 :: K -> Type] [fr :: K -> Type] [t ::: Type]
    (f : p :: K -> f1 p -> f2 p -> fr p -> t)
    [r ::: {K}] (fl : folder r) (v1 : variant (map f1 r)) (v2 : variant (map f2 r)) (r : $(map fr r)) : option t =
    match v1
    (@Top.foldR [fr] [fn r => others :: {K} -> [others ~ r] =>
                     folder (r ++ others)
                     -> variant (map f2 (r ++ others))
                     -> $(map (fn p => f1 p -> option t) r)]
     (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (meta : fr p)
         (acc : others :: {K} -> [others ~ r] =>
          folder (r ++ others)
          -> variant (map f2 (r ++ others))
          -> $(map (fn p => f1 p -> option t) r))
         [others :: {K}] [others ~ [nm = p] ++ r]
         (fl : folder ([nm = p] ++ r ++ others))
         (v2 : variant (map f2 ([nm = p] ++ r ++ others))) =>
         {nm = fn x1 => match v2
                        ({nm = fn x2 => Some (f [p] x1 x2 meta)}
                             ++ (@map0 [fn p => f2 p -> option t] (fn [p' ::_] _ => None) fl -- nm))}
             ++ @acc [[nm = p] ++ others] ! fl v2)
     (fn [others ::_] [others ~ []] _ _ => {})
     fl r [[]] ! fl v2)

fun testEq [K] [f :: K -> Type] [nm :: Name] [t ::: K] [ts ::: {K}] [r ::: {K}] [[nm] ~ ts]
    (pf : Eq.eq r ([nm = t] ++ ts)) (fl : folder r) (v : variant (map f r)) : option (f t) =
  @test [nm] ! (@Folder.mp (@Eq.cast pf [folder] fl))
   (Eq.cast pf [fn r => variant (map f r)] v)

fun makeEq [K] [f :: K -> Type] [nm :: Name] [t ::: K] [ts ::: {K}] [r ::: {K}] [[nm] ~ ts]
    (pf : Eq.eq r ([nm = t] ++ ts)) (x : f t) : variant (map f r) =
  Eq.cast (Eq.sym pf) [fn r => variant (map f r)] (make [nm] x)

con variantMake ts' ts = $(map (fn t => t -> variant ts') ts)
con mkLabelsAccum r = s :: {Type} -> [r ~ s] => variantMake (r ++ s) r
fun mkLabels [ts ::: {Type}] (fl : folder ts) : variantMake ts ts
  = @Top.fold [mkLabelsAccum]
          (fn [nm::_] [v::_] [r::_] [[nm] ~ r]
              (k : mkLabelsAccum r)
              [s::_] [[nm = v] ++ r ~ s] => k [[nm = v] ++ s] ++ {nm = make [nm]})
          (fn [s::_] [[] ~ s] => {}) fl [[]] !

con type_case ts t a = (a -> variant ts) -> a -> t

fun declareCase [ts] [t] [a] (f : (a -> variant ts) -> a -> t) : type_case ts t a = f
fun typeCase [ts] [t] (v : variant ts) (dstrs : $(map (type_case ts t) ts)) (fl : folder ts) : t
(* Ur/Web not clever enough to calculate these folders, it seems *)
  = match v (@Record.ap [fn a => a -> variant ts] [fn a => a -> t] fl dstrs (@mkLabels fl))
