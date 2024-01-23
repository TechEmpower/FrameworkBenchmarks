type world = { id : int; randomNumber : int }

type fortune = { id : int; message : string }

let compare_fortune a b = String.compare a.message b.message
