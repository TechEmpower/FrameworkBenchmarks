fun numFields [r ::: {Type}] (fl : folder r) (r : $r) : int =
    @fold [fn _ => int] (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] acc => acc+1) 0 fl

fun mem [a ::: Type] [ns ::: {Unit}] (_ : eq a) (fl : folder ns) (x : a) (r : $(mapU a ns)) : bool =
    @foldUR [a] [fn _ => bool]
     (fn [nm ::_] [r ::_] [[nm] ~ r] y acc =>
         acc || x = y)
     False fl r

fun equal [ts ::: {Type}] (eqs : $(map eq ts)) (fl : folder ts) (r1 : $ts) (r2 : $ts) : bool =
    @foldR3 [eq] [ident] [ident] [fn _ => bool]
     (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] isEq x y acc =>
         acc && @eq isEq x y)
     True fl eqs r1 r2

fun ap [K] [tf1 :: K -> Type] [tf2 :: K -> Type]
       [r ::: {K}] (fl : folder r) (fs : $(map (fn t => tf1 t -> tf2 t) r)) (xs : $(map tf1 r))
  = @map2 [fn t => tf1 t -> tf2 t] [fn t => tf1 t] [tf2] (fn [t] f x => f x) fl fs xs
