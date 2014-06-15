con incl' = K ==> fn (r1 :: {K}) (r2 :: {K}) (r' :: {K}) =>
                     [r1 ~ r'] => {Expose : f :: ({K} -> Type) -> f r2 -> f (r1 ++ r'),
                                   Hide : f :: ({K} -> Type) -> f (r1 ++ r') -> f r2}

con incl = K ==> fn (r1 :: {K}) (r2 :: {K}) =>
                    tp :: Type -> (r' :: {K} -> [r1 ~ r'] => incl' r1 r2 r' -> tp) -> tp

fun incl [K] [r1 :: {K}] [r2 :: {K}] [r1 ~ r2] =
 fn [tp :: Type] (f : r' :: {K} -> [r1 ~ r'] => incl' r1 (r1 ++ r2) r' -> tp) =>
    f [r2] (fn [r1 ~ r2] => {Expose = fn [f :: ({K} -> Type)] x => x,
                             Hide = fn [f :: ({K} -> Type)] x => x})
    
fun proj [r1 ::: {Type}] [r2 ::: {Type}] (i : incl r1 r2) (r : $r2) =
    i [$r1] (fn [r' :: {Type}] [r1 ~ r'] (i' : incl' r1 r2 r') =>
                i'.Expose [fn r => $r] r --- r')

fun inv1 [K] [nm :: Name] [t :: K] [r :: {K}] [r' :: {K}] [[nm] ~ r]
         [f :: Name -> K -> {K} -> Type]
         (i : incl ([nm = t] ++ r) r')
         (f : nm :: Name -> t :: K -> r :: {K} -> [[nm] ~ r] => f nm t ([nm = t] ++ r)) =
    i [f nm t r'] (fn [r'' :: {K}] [[nm = t] ++ r ~ r''] (i' : incl' ([nm = t] ++ r) r' r'') =>
                      i'.Hide [f nm t] (f [nm] [t] [r ++ r'']))

fun inv2 [K] [nm :: Name] [t :: K] [r :: {K}] [r' :: {K}] [[nm] ~ r]
         (i : incl ([nm = t] ++ r) r') =
    i [incl r r'] (fn [r'' :: {K}] [[nm = t] ++ r ~ r''] (i' : incl' ([nm = t] ++ r) r' r'') =>
                   fn [tp :: Type] (f : r''' :: {K} -> [r ~ r'''] => incl' r r' r''' -> tp) =>
                      f [[nm = t] ++ r''] (fn [r ~ [nm = t] ++ r''] =>
                                              {Expose = fn [f :: ({K} -> Type)] (x : f r') => i'.Expose [f] x,
                                               Hide = fn [f :: ({K} -> Type)] x => i'.Hide [f] x}))

fun fold [K] [tf :: {K} -> Type] [r ::: {K}]
         (f : nm :: Name -> v :: K -> r' :: {K}
              -> [[nm] ~ r'] => incl ([nm = v] ++ r') r -> tf r' -> tf ([nm = v] ++ r'))
         (i : tf []) (fl : folder r) =
    @Top.fold [fn r' => incl r' r -> tf r']
     (fn [nm :: Name] [v :: K] [r' :: {K}] [[nm] ~ r'] acc i =>
         f [nm] [v] [r'] i (acc (inv2 [nm] [v] [r'] [r] i)))
     (fn _ => i)
     fl (incl [r] [[]])
