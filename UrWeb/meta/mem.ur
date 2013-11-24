con mem' = K ==> fn (nm :: Name) (t :: K) (r :: {K}) (r' :: {K}) =>
    [[nm] ~ r'] => {Expose : f :: ({K} -> Type) -> f r -> f ([nm = t] ++ r'),
                    Hide : f :: ({K} -> Type) -> f ([nm = t] ++ r') -> f r}

con mem = K ==> fn (nm :: Name) (t :: K) (r :: {K}) =>
                   tp :: Type -> (r' :: {K} -> [[nm] ~ r'] => mem' nm t r r' -> tp) -> tp

fun mem [K] [nm :: Name] [t :: K] [r :: {K}] [[nm] ~ r] =
    fn [tp :: Type] (f : r' :: {K} -> [[nm] ~ r'] => mem' nm t ([nm = t] ++ r) r' -> tp) =>
       f [r] (fn [[nm] ~ r] => {Expose = fn [f :: {K} -> Type] x => x,
                                Hide = fn [f :: {K} -> Type] x => x})

fun mp [K] [K2] [f :: K -> K2] [nm ::: Name] [t ::: K] [r ::: {K}] (m : mem nm t r) =
    m [mem nm (f t) (map f r)] (fn [r' :: {K}] [[nm] ~ r'] (m' : mem' nm t r r') =>
                                fn [tp :: Type] (f : r' :: {K2} -> [[nm] ~ r'] =>
                                                 mem' nm (f t) (map f r) r' -> tp) =>
                                   f [map f r'] (fn [[nm] ~ map f r'] =>
                                                    {Expose = fn [f' :: {K2} -> Type] x =>
                                                                 m'.Expose [fn r => f' (map f r)] x,
                                                     Hide = fn [f' :: {K2} -> Type] x =>
                                                               m'.Hide [fn r => f' (map f r)] x}))

fun proj [nm ::: Name] [t ::: Type] [r ::: {Type}] (m : mem nm t r) (r : $r) =
    m [t] (fn [r' :: {Type}] [[nm] ~ r'] (m' : mem' nm t r r') =>
              (m'.Expose [fn r => $r] r).nm)

fun replace [nm ::: Name] [t ::: Type] [r ::: {Type}] (m : mem nm t r) (r : $r) (v : t) =
    m [$r] (fn [r' :: {Type}] [[nm] ~ r'] (m' : mem' nm t r r') =>
               m'.Hide [fn r => $r] (m'.Expose [fn r => $r] r -- nm ++ {nm = v}))

fun fold [K] [tf :: ({K} -> Type)] [r ::: {K}]
    (f : nm :: Name -> v :: K -> r' :: {K} -> [[nm] ~ r']
     => mem nm v r -> tf r' -> tf ([nm = v] ++ r'))
    (i : tf []) (fl : folder r) =
    @@Incl.fold [tf] [r]
      (fn [nm :: Name] [v :: K] [r' :: {K}] [[nm] ~ r'] (i : Incl.incl ([nm = v] ++ r') r) acc =>
          f [nm] [v] [r'] (Incl.inv1 [nm] [v] [r'] [r] [mem] i mem) acc)
      i fl
