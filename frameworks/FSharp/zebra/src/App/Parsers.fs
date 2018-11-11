module Parsers

open System
open NonStructuralComparison // needed for parser performance, non boxing of struct equality
open OptimizedClosures       // needed to apply multi-curry args at once with adapt (invoke method)

type Range =
    struct
        val Start  : int
        val Finish : int
    new(s,f)={Start=s;Finish=f}
    end

type ValueOption<'T> =
    struct
        val HasValue : bool
        val Value: 'T
    new(a,b) = {HasValue = a; Value = b}
    end
let inline VSome v = ValueOption<_>(true,v)
let inline VNone () = ValueOption<'T>(false,Unchecked.defaultof<'T>)


type Parser = FSharpFunc<string, int, int, ValueOption<obj>>

let inline private between x l u = (x - l) * (u - x) >= LanguagePrimitives.GenericZero

let inline private rtrn (o : 'T) = ValueOption<'T>(true, o)
let inline private failure () = ValueOption<'T>(false,Unchecked.defaultof<'T>)

/// Private Range Parsers that quickly try parse over matched range (all r.Finish checked before running in preceeding functions)

let stringParse (path : string,r:Range) = path.Substring(r.Start, r.Finish - r.Start + 1) |> rtrn

let charParse (path : string,r:Range) = path.[r.Start] |> rtrn // this is not ideal method (but uncommonly used)
let boolParse (path : string,r:Range) =
    match path.Substring(r.Start, r.Finish - r.Start) with
    | "true"  | "True"  | "TRUE"  -> true  |> rtrn
    | "false" | "False" | "FALSE" -> false |> rtrn
    | _ -> failure ()

let intParse (path : string,r:Range) =
    let mutable result = 0
    let mutable negNumber = false
    let rec go pos =
        let charDiff = int path.[pos] - int '0'
        if between charDiff 0 9 then
            result <- (result * 10) + charDiff
            if pos = r.Finish then
                if negNumber then - result else result
                |> rtrn
            else go (pos + 1)       // continue iter
        else failure ()
    //Start Parse taking into account sign operator
    match path.[r.Start] with
    | '-' -> negNumber <- true ; go (r.Start + 1)
    | '+' -> go (r.Start + 1)
    | _   -> go (r.Start)

let int64Parse (path : string,r:Range) =
    let mutable result = 0L
    let mutable negNumber = false
    let rec go pos =
        let charDiff = int64 path.[pos] - int64 '0'
        if between charDiff 0L 9L then
            result <- (result * 10L) + charDiff
            if pos = r.Finish then
                if negNumber then - result |> rtrn else result |> rtrn
            else go (pos + 1)       // continue iter
        else failure ()
    //Start Parse taking into account sign operator
    match path.[r.Start] with
    | '-' -> negNumber <- true ; go (r.Start + 1)
    | '+' -> go (r.Start + 1)
    | _   -> go (r.Start)

let private decDivide =
    [| 1.; 10.; 100.; 1000.; 10000.; 100000.; 1000000.; 10000000.; 100000000.; 100000000. |]
    |> Array.map (fun d -> 1. / d) // precompute inverse once at compile time

let floatParse (path : string,r:Range) =
    let mutable result    = 0.
    let mutable decPlaces = 0
    let mutable negNumber = false

    let rec go pos =
        if path.[pos] = '.' then
            decPlaces <- 1
            if pos < r.Finish then go (pos + 1) else failure ()
        else
            let charDiff = float path.[pos] - float '0'
            if between charDiff 0. 9. then
                if decPlaces = 0 then
                    result <- (result * 10.) + charDiff
                else
                    //result <- result + charDiff
                    result <- result + (charDiff * decDivide.[decPlaces]) // char is divided using multiplication of pre-computed divisors
                    decPlaces <- decPlaces + 1
                if pos = r.Finish || decPlaces > 9 then
                    if negNumber then - result else result
                    |> rtrn
                else go (pos + 1)   // continue iter
            else failure () // Invalid Character in path

    //Start Parse taking into account sign operator
    match path.[r.Start] with
    | '-' -> negNumber <- true ; go (r.Start + 1)
    | '+' -> go (r.Start + 1)
    | _   -> go (r.Start)

let private guidMap = [| 3; 2; 1; 0; 5; 4; 7; 6; 8; 9; 10; 11; 12; 13; 14; 15 |]

let guidParse (path : string,r:Range) =
    let byteAry = Array.zeroCreate<byte>(16)
    let mutable bytePos = 0
    let mutable byteCur = 0uy
    let mutable atHead  = true
    let rec go pos =
        if path.[pos] = '-' then // skip over '-' chars
            if pos < r.Finish then go (pos + 1) else failure ()
        else
            let cv =  byte path.[pos]

            let value =
                if  cv >= byte '0' then
                    if cv <= byte '9' then cv - byte '0'
                    elif cv >= byte 'A' then
                        if cv <= byte 'F' then cv - byte 'A' + 10uy
                        elif cv >= byte 'a' then
                            if cv <= byte 'f' then cv - byte 'a' + 10uy
                            else 255uy
                        else 255uy
                    else 255uy
                else 255uy

            if value = 255uy then
                failure ()
            else
                if atHead then
                    byteCur <- value <<< 4
                    atHead  <- false
                    go (pos + 1)   // continue iter
                else
                    byteAry.[guidMap.[bytePos]] <- byteCur ||| value
                    if bytePos = 15 then
                        Guid(byteAry) |> rtrn
                    else
                        byteCur <- 0uy
                        atHead  <- true
                        bytePos <- bytePos + 1
                        go (pos + 1)   // continue iter
    //Start Parse
    go (r.Start)

type Parse = Parse with
    static member inline ($) (Parse, _:string) : _ -> ValueOption<string> = stringParse
    static member inline ($) (Parse, _:int)    : _ -> ValueOption<int> = intParse
    static member inline ($) (Parse, _:float)  : _ -> ValueOption<float> = floatParse
    static member inline ($) (Parse, _:bool)   : _ -> ValueOption<bool>  = boolParse
    static member inline ($) (Parse, _:Guid)   : _ -> ValueOption<Guid>  = guidParse
    static member inline ($) (Parse, _:int64)   : _ -> ValueOption<int64>  = int64Parse
    static member inline ($) (Parse, _:char)   : _ -> ValueOption<char>  = charParse
