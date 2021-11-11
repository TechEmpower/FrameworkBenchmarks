module Router

open System.Collections.Generic
open System.Text
open Printf
open OptimizedClosures       // needed to apply multi-curry args at once with adapt (invoke method)
open Microsoft.AspNetCore.Http
open Parsers
open State
open ExecNodes
open RouteNode


//type State<'T> -> unit = State<'T> -> unit


// RouteNodes
//////////////////
type ParseFn<'a> = string * Range -> ValueOption< 'a> 

type RouteNode1<'T,'a>(inext:INode<'T>,ifail:INode<'T>,current:'a -> State<'T> -> unit,parseA:ParseFn<'a>) =
    let mutable next = inext
    let mutable fail = ifail
    //let parseA : string * Range -> ValueOption< ^a> = Parse $ Unchecked.defaultof< ^a>
    let fnOpt = FSharpFunc<_,_,_>.Adapt current
    
    // Parse like a specialised Apply so routing functions need concrete type
    interface IRouteNode<'T> with
        member x.Parse (range:Range [],ctx: State<'T>) : bool =
            let path = ctx.HttpContext.Request.Path.Value
            let v1 = parseA(path,range.[0])
            if v1.HasValue then
                ctx.DNode <- x 
                fnOpt.Invoke(v1.Value,ctx)
                true
            else false

    interface INode<'T> with
        member __.Next with get () = next and set v = next <- v
        member __.Fail with get () = fail and set v = fail <- v
        member x.Apply (_) = failwith "Who's calling apply on a route node !? ya dipshit"

///////////////////      

type RouteNode2<'T,'a,'b>(inext:INode<'T>,ifail:INode<'T>,fn:^a -> ^b -> State<'T> -> unit,
                            parseA:ParseFn<'a>,parseB:ParseFn<'b>) =
    let mutable next = inext
    let mutable fail = ifail
    let fnOpt = FSharpFunc<_,_,_,_>.Adapt fn
    // Parse like a specialised Apply so routing functions need concrete type
    interface IRouteNode<'T> with 
        member x.Parse (range:Range [],ctx:State<'T>) =
            let path = ctx.HttpContext.Request.Path.Value
            let v1 = parseA(path,range.[0])
            if v1.HasValue then
                let v2 = parseB(path,range.[1])
                if v2.HasValue then
                    ctx.DNode <- x
                    fnOpt.Invoke(v1.Value,v2.Value,ctx)
                    true
                else false    
            else false

    interface INode<'T> with
        member __.Next with get () = next and set v = next <- v
        member __.Fail with get () = fail and set v = fail <- v
        member x.Apply (_) = failwith "Who's calling apply on a route node !? ya dipshit"    

type RouteNode3<'T,'a,'b,'c>(inext:INode<'T>,ifail:INode<'T>,fn:'a -> 'b -> 'c -> State<'T> -> unit,
                                parseA:ParseFn<'a>,parseB:ParseFn<'b>,parseC:ParseFn<'c>) =
    let mutable next = inext
    let mutable fail = ifail
    let fnOpt = FSharpFunc<_,_,_,_,_>.Adapt fn
    // Parse like a specialised Apply so routing functions need concrete type
    interface IRouteNode<'T> with 
        member x.Parse (range:Range [],ctx:State<'T>) =
            let path = ctx.HttpContext.Request.Path.Value
            let v1 = parseA(path,range.[0])
            if v1.HasValue then
                let v2 = parseB(path,range.[1])
                if v2.HasValue then
                    let v3 = parseC(path,range.[2])
                    if v3.HasValue then
                        ctx.DNode <- x
                        fnOpt.Invoke(v1.Value,v2.Value,v3.Value,ctx)
                        true
                    else false
                else false    
            else false

    interface INode<'T> with
        member __.Next with get () = next and set v = next <- v
        member __.Fail with get () = fail and set v = fail <- v
        member x.Apply (_) = failwith "Who's calling apply on a route node !? ya dipshit"    


type RouteNode4<'T,'a,'b,'c,'d>(inext:INode<'T>,ifail:INode<'T>,fn:'a -> 'b -> 'c -> 'd -> State<'T> -> unit,
                                parseA:ParseFn<'a>,parseB:ParseFn<'b>,parseC:ParseFn<'c>,parseD:ParseFn<'d>) =
    let mutable next = inext
    let mutable fail = ifail
    let fnOpt = FSharpFunc<_,_,_,_,_,_>.Adapt fn
    // Parse like a specialised Apply so routing functions need concrete type
    interface IRouteNode<'T> with 
        member x.Parse (range:Range [],ctx:State<'T>) =
            let path = ctx.HttpContext.Request.Path.Value
            let v1 = parseA(path,range.[0])
            if v1.HasValue then
                let v2 = parseB(path,range.[1])
                if v2.HasValue then
                    let v3 = parseC(path,range.[2])
                    if v3.HasValue then
                        let v4 = parseD(path,range.[3])
                        if v4.HasValue then
                            ctx.DNode <- x
                            fnOpt.Invoke(v1.Value,v2.Value,v3.Value,v4.Value,ctx)
                            true
                        else false
                    else false
                else false    
            else false

    interface INode<'T> with
        member __.Next with get () = next and set v = next <- v
        member __.Fail with get () = fail and set v = fail <- v
        member x.Apply (_) = failwith "Who's calling apply on a route node !? ya dipshit"    


// --------------------------------------
// Routing Node Map Functions used to build trie
// --------------------------------------


// ** explicit individual cases for each method & #no of params needed to enforce types between PrintfFormat & parse functions, composition ternary operator works creating these temporary classes 

type RouteBase<'T>(method:METHODS,pattern:string) =
    member __.Method with get () = method
    member __.Pattern with get () = pattern

type RouteBase<'T,'a>(method:METHODS,pattern:string) =
    member __.Method with get () = method
    member __.Pattern with get () = pattern

type RouteBase<'T,'a,'b>(method:METHODS,pattern:string) =
    member __.Method with get () = method
    member __.Pattern with get () = pattern

type RouteBase<'T,'a,'b,'c>(method:METHODS,pattern:string) =
    member __.Method with get () = method
    member __.Pattern with get () = pattern

type RouteBase<'T,'a,'b,'c,'d>(method:METHODS,pattern:string) =
    member __.Method with get () = method
    member __.Pattern with get () = pattern



// base route base map function
let route (method:METHODS) (path:string) = 
    fun (next:INode<'T>,fail:INode<'T>) (root:RNode<'T>) ->
    // Simple route that iterates down nodes and if function found, execute as normal
        RNode.ExtendPath root path ( HandlerMap(method,next) )

// // Method filtered route 

let inline get (path:string)    = RouteBase<'T>( METHODS.GET,path)
let inline post (path:string)   = RouteBase<'T>( METHODS.POST, path)
let inline delete (path:string) = RouteBase<'T>( METHODS.DELETE, path)
let inline put (path:string)    = RouteBase<'T>( METHODS.PUT, path)
let inline patch (path:string)  = RouteBase<'T>( METHODS.PATCH, path)



// --------------------------------------
// Helper Functions
// --------------------------------------

// temporary compose out handler to allow composition out of route functions, same as wraping in () or using <|
//let inline (=>) (a:HttpHandler -> Node -> Node) (b:HttpHandler) = a b

let inline private addCharArray (c:char) (ary:char []) =
    if ary |> Array.exists (fun v -> v = c) then
        ary
    else
        let nAry = Array.zeroCreate<_>(ary.Length + 1)
        Array.blit ary 0 nAry 0 ary.Length
        nAry.[ary.Length] <- c
        nAry

// helper to get child node of same match format (slow for now, needs optimisation)
let inline private getPostMatchNode argCount pcount (fmt:char) (nxt:char) (ils:Cont<'T> list) =
    let rec go (ls:Cont<'T> list) (acc:Cont<'T> list) (no:RNode<'T> ValueOption) =
        match ls with
        | [] ->
            if no.HasValue then
                no.Value, acc |> List.sortBy (fun fn -> fn.Precedence)
            else
                let n = RNode("")
                if pcount = 0 then
                    n ,(InitialMatch(argCount,fmt,[|nxt|],n)) :: acc |> List.sortBy (fun fn -> fn.Precedence) // lets runtime know how big a range array to allocate
                else
                    n ,(ApplyMatch(pcount,fmt,[|nxt|],n)) :: acc |> List.sortBy (fun fn -> fn.Precedence) // the parameter count will let runtime know where to slot in range 
        | hfn :: tfns ->
            match hfn with
            | ApplyMatch (pcount',f,ncl,n) ->
                if f = fmt then
                    let nncl = addCharArray nxt ncl
                    go tfns (ApplyMatch(pcount',f,nncl,n)::acc) (VSome n)
                    // finished as found matched format but need to complete acc list
                else go tfns (hfn::acc) no
            | _ -> go tfns (hfn::acc) no
    go ils [] (VNone ())



// base parameter parsing map apply funciton
let routef (method:METHODS) (pattern : string) (fn:IRouteNode<'T>) (argCount:int) (root:RNode<'T>) =

// parsing route that iterates down nodes, parses, and then continues down further notes if needed
    let last = pattern.Length - 1

    let rec go (i:int,ts:int,pcount,node:RNode<'T>) =
        node.MethodFilters.Add method |> ignore

        let pl = pattern.IndexOf('%',i)
        if pl < 0 || pl = last then
            //Match Complete (no futher parse '%' chars
            if pcount = 0 then
                failwith "'routef' (route Parse) used with no arguments? please add % format args or change to simple 'route' for non-parse routes"
            else
                RNode.ExtendPath node (pattern -| ts) ( Complete( method,fn )) 
        else
            let fmtChar = pattern.[pl + 1]
            // overrided %% -> % case
            if fmtChar = '%' then
                //keep token start (+1 just one %), skip
                go(pl + 2, ts + 1,pcount, node)
            // formater with valid key
            else if validFormats.Contains fmtChar then

                if pl + 1 = last then // if finishes in a parse
                    // if node.MidFns |> List.exists (function | ApplyMatchAndComplete(c,_,_) -> fmtChar = c | _ -> false )
                    // then sprintf "duplicate paths detected '%s', Trie Build skipping..." pattern |> failwith
                    // else
                        let nnode = RNode.ExtendPath node (pattern.Substring(ts,pl - ts)) (MatchComplete( method,pcount,fn ))
                        nnode.MethodFilters.Add method |> ignore
                        nnode
                else //otherwise add mid pattern parse apply
                    //get node this parser will be on
                    let nnode = RNode.ExtendPath node (pattern.Substring(ts,pl - ts)) Empty
                    let cnode,midFns = getPostMatchNode argCount pcount fmtChar pattern.[pl+2] nnode.MidFns
                    nnode.MidFns <- midFns //update adjusted functions
                    nnode.MethodFilters.Add method |> ignore
                    go(pl + 2, pl + 2, pcount + 1, cnode)
            // badly formated format string that has unknown char after %
            else
                failwith (sprintf "Routef parsing error, invalid format char identifier '%c' , should be: b | c | s | i | d | f" fmtChar)
                go(pl + 1, ts, pcount, node)

    go(0, 0, 0, root)




// GET Functions
////////////////////////////////

/// **get1**: GET Method filtered route with **one** parameter to be parsed and applied
let inline get1 (fmt:PrintfFormat< ^a -> State<'T> -> unit,_,_,State<'T> -> unit>) = 
    RouteBase<'T, ^a>(METHODS.GET,fmt.Value)

/// **get2**: GET Method filtered route with **two** parameter to be parsed and applied
let inline get2 (fmt:PrintfFormat< ^a -> ^b -> State<'T> -> unit,_,_,State<'T> -> unit>) = 
    RouteBase<'T, ^a, ^b>(METHODS.GET,fmt.Value)

/// **get3**: GET Method filtered route with **three** parameter to be parsed and applied
let inline get3 (fmt:PrintfFormat< ^a -> ^b -> ^c -> State<'T> -> unit,_,_,State<'T> -> unit>) =
    RouteBase<'T, ^a, ^b, ^c>(METHODS.GET,fmt.Value)

/// **get4**: GET Method filtered route with **four** parameter to be parsed and applied
let inline get4 (fmt:PrintfFormat< ^a -> ^b -> ^c -> ^d -> State<'T> -> unit,_,_,State<'T> -> unit>) =
    RouteBase<'T, ^a, ^b, ^c>(METHODS.GET,fmt.Value)


// POST Functions
//////////////////////////////////

/// **post1**: POST Method filtered route with **one** parameter to be parsed and applied
let inline post1 (fmt:PrintfFormat< ^a -> State<'T> -> unit,_,_,State<'T> -> unit>) = 
    RouteBase<'T, ^a>(METHODS.POST,fmt.Value)

/// **post2**: POST Method filtered route with **two** parameter to be parsed and applied
let inline post2 (fmt:PrintfFormat< ^a -> ^b -> State<'T> -> unit,_,_,State<'T> -> unit>) = 
    RouteBase<'T, ^a, ^b>(METHODS.POST,fmt.Value)

/// **post3**: POST Method filtered route with **three** parameter to be parsed and applied
let inline post3 (fmt:PrintfFormat< ^a -> ^b -> ^c -> State<'T> -> unit,_,_,State<'T> -> unit>) (fn: ^a -> ^b -> ^c -> State<'T> -> unit) =
    RouteBase<'T, ^a, ^b, ^c>(METHODS.POST,fmt.Value)

/// **post4**: POST Method filtered route with **four** parameter to be parsed and applied
let inline post4 (fmt:PrintfFormat< ^a -> ^b -> ^c -> ^d -> State<'T> -> unit,_,_,State<'T> -> unit>) (fn: ^a -> ^b -> ^c -> ^d -> State<'T> -> unit) =
    RouteBase<'T, ^a, ^b, ^c>(METHODS.POST,fmt.Value)

// PUT Functions
///////////////////////////////////

/// **put1**: PUT Method filtered route with **one** parameter to be parsed and applied
let inline put1 (fmt:PrintfFormat< ^a -> State<'T> -> unit,_,_,State<'T> -> unit>) = 
    RouteBase<'T, ^a>(METHODS.PUT,fmt.Value)

/// **put2**: PUT Method filtered route with **two** parameter to be parsed and applied
let inline put2 (fmt:PrintfFormat< ^a -> ^b -> State<'T> -> unit,_,_,State<'T> -> unit>) = 
    RouteBase<'T, ^a, ^b>(METHODS.PUT,fmt.Value)

/// **put3**: PUT Method filtered route with **three** parameter to be parsed and applied
let inline put3 (fmt:PrintfFormat< ^a -> ^b -> ^c -> State<'T> -> unit,_,_,State<'T> -> unit>) (fn: ^a -> ^b -> ^c -> State<'T> -> unit) =
    RouteBase<'T, ^a, ^b, ^c>(METHODS.PUT,fmt.Value)

/// **put4**: PUT Method filtered route with **four** parameter to be parsed and applied
let inline put4 (fmt:PrintfFormat< ^a -> ^b -> ^c -> ^d -> State<'T> -> unit,_,_,State<'T> -> unit>) (fn: ^a -> ^b -> ^c -> ^d -> State<'T> -> unit) =
    RouteBase<'T, ^a, ^b, ^c>(METHODS.PUT,fmt.Value)


// Runtime Processor
/////////////////////////////////////////

type checkCompletionPathResult<'T> =
    struct
        val Success :bool
        val Position : int
        val Node : RNode<'T>
    new(a,b,c) = {Success = a;Position = b;Node = c}    
    end

type getNodeCompletionResult<'T> =
    struct
        val Success :bool
        val Prend : int
        val Nxtpos : int 
        val Nxtnode : RNode<'T>
    new(a,b,c,d) = {Success = a;Prend = b;Nxtpos = c;Nxtnode =d}  
    end

let private emptyRange = Unchecked.defaultof<Range []>

let processPath (abort:INode<'T>) (root:RNode<'T>) : State<'T> -> unit =

    fun ctx ->

        //let abort  = setStatusCode 404 >=> text "Not found"

        let path : string = ctx.HttpContext.Request.Path.Value
        let last = path.Length - 1
        let method = 
            match ctx.HttpContext.Request.Method with
            | "GET"     -> METHODS.GET
            | "POST"    -> METHODS.POST 
            | "PUT"     -> METHODS.PUT
            | "DELETE"  -> METHODS.DELETE
            | "PATCH"   -> METHODS.PATCH 
            | _ -> METHODS.UNKNOWN 
            

        

        let rec checkCompletionPath (pos:int,node:RNode<'T>) = // this funciton is only used by parser paths
            //this function doesn't test array bounds as all callers do so before
            let inline success(pos,node) = checkCompletionPathResult(true,pos,node)                         // todo: move out under type
            let inline failure(pos)      = checkCompletionPathResult(false,pos,Unchecked.defaultof<RNode<'T>>)   // todo: move out under type

            if commonPathIndex path pos node.Token = node.Token.Length then
                let nxtChar = pos + node.Token.Length
                if (nxtChar - 1) = last then //if this pattern match shares node chain as substring of another
                    if node.EndFns.IsEmpty
                    then failure pos //pos, None
                    else success(nxtChar,node) //nxtChar, Some node
                else
                    match node.TryGetValue path.[nxtChar] with
                    | true, cnode ->
                        checkCompletionPath(nxtChar,cnode)
                    | false, _ ->
                        // no further nodes, either a static url didnt match or there is a pattern match required
                        if node.MidFns.IsEmpty
                        then failure pos
                        else success(nxtChar,node)
            else failure pos

        /// (next match chars,pos,match completion node) -> (parse end,pos skip completed node,skip completed node) option
        let rec getNodeCompletion (cs:char [], pos ,node:RNode<'T>) =
            let inline success(prend,nxtpos,nxtnode) = getNodeCompletionResult (true,prend,nxtpos,nxtnode)              // todo: move out under type
            let inline failure ()                    = getNodeCompletionResult (false,0,0,Unchecked.defaultof<RNode<'T>>)    // todo: move out under type

            match path.IndexOfAny(cs,pos) with // jump to next char ending (possible instr optimize vs node +1 crawl)
            | -1 -> failure ()
            | x1 -> //x1 represents position of match close char but rest of chain must be confirmed
                let cp = checkCompletionPath(x1,node) 
                if cp.Success 
                then success(x1 - 1,cp.Position,cp.Node)                 // from where char found to end of node chain complete
                else getNodeCompletion(cs, x1 + 1, node) // char foundpart of match, not completion string


        //let createResult (args:obj list) (argCount:int) (pfc:ParseFnCache) =


        let rec processEnd (fns:Cont<'T> list, pos, range:Range []) : bool =
            match fns with
            | [] -> false
            | h :: t ->
                match h with
                | HandlerMap(method,inode) ->
                    if methodMatch(ctx,method) then
                        ctx.PathPosition <- pos
                        inode.Apply (ctx)
                        true
                    else false
                | Complete (method,fn) -> 
                    if methodMatch(ctx,method) then
                        ctx.PathPosition <- pos
                        if fn.Parse(range,ctx) then true
                        else processEnd (t,pos,range)
                    else false
                | x -> failwithf "Cont Mapping failed: %A in processEnd" x                    

        let rec processMid (fns:Cont<'T> list,pos, range) =

            let inline applyMatchAndComplete pos pcount (range:Range []) (fn:IRouteNode<'T>) tail =
                range.[pcount] <- Range(pos,last)
                ctx.PathPosition <- pos
                if fn.Parse(range,ctx) then true
                else processMid(tail, pos, range)
                 
            let rec applyMatch pos pcount (f:char) (ca:char[]) n (range:Range []) tail  =
                let nc = getNodeCompletion(ca, pos, n) 
                match nc.Success with
                | true -> //,fpos,npos,cnode)
                    range.[pcount] <- Range(pos, nc.Prend)                    
                    
                    if nc.Nxtpos - 1 = last then //if have reached end of path through nodes, run HandlerFn
                        processEnd(nc.Nxtnode.EndFns, nc.Nxtpos, range )
                    else
                        processMid(nc.Nxtnode.MidFns, nc.Nxtpos, range )
                | false -> processMid(tail, pos, range) // subsequent match could not complete so fail


            let inline InitialMatch argCount fmt nextChars node tail =
                let range = Array.zeroCreate<Range> argCount    // Allocate range cursor
                applyMatch pos 0 fmt nextChars node range tail  // Apply match using 0 inital paramter position

            match fns with
            | [] -> false
            | h :: t ->
                match h with
                | InitialMatch (argCount,fmt,nextChars,node) -> InitialMatch argCount fmt nextChars node t                 
                | ApplyMatch (pcount,fmt,nexts,node) -> applyMatch pos pcount fmt nexts node range t
                | MatchComplete (method,pcount,fn) -> 
                    if methodMatch(ctx,method) then
                        if pcount = 0 then
                            applyMatchAndComplete pos pcount [|Range(pos,last)|] fn t  //<< HACK
                        else
                            applyMatchAndComplete pos pcount range fn t 
                    else false
                | x -> failwithf "Cont Mapping failed: %A in processMid" x 

        let rec crawl (pos:int , node:RNode<'T>) : bool =
            if node.MethodFilters.Count > 0 && not (node.MethodFilters.Contains method) then
                    false
            else
                if node.Token.Length > 0 then
                    let cp = commonPathIndex path pos node.Token
                    if cp = node.Token.Length then
                        let nxtChar = pos + node.Token.Length
                        if (nxtChar - 1 ) = last then //if have reached end of path through nodes, run HandlerFn
                            processEnd(node.EndFns, pos, emptyRange )
                        else
                            match node.TryGetValue path.[nxtChar] with
                            | true, cnode ->
                                if (pos + cnode.Token.Length ) = last then //if have reached end of path through nodes, run HandlerFn
                                    processEnd(cnode.EndFns, pos + node.Token.Length, emptyRange )
                                else                //need to continue down chain till get to end of path
                                    crawl (nxtChar,cnode)
                            | false, _ ->
                                // no further nodes, either a static url didnt match or there is a pattern match required
                                processMid( node.MidFns, nxtChar, emptyRange )
                    else
                        false
                elif node.Token.Length = 0 then
                    match node.TryGetValue path.[pos] with
                    | true, cnode ->
                        crawl (pos,cnode)
                    | false, _ ->
                        // no further nodes, either a static url didnt match or there is a pattern match required
                        processMid( node.MidFns, pos , emptyRange )
                else
                    //printfn ">> failed to match %s path with %s token, commonPath=%i" (path.Substring(pos)) (node.Token) (commonPathIndex path pos node.Token)
                    false

        // begin path crawl process
        if crawl(ctx.PathPosition,root) then () 
        else abort.Apply(ctx)


type RouterNode<'T>(inext:INode<'T>,ifail:INode<'T>,routes:((INode<'T> * INode<'T>) -> RNode<'T> -> RNode<'T>) list) =

    let mutable next = inext
    let mutable fail = ifail

    let inode = RNode("")    // Create a new base node for each route group, state pathpos allows autonomy

    do // build out the route tree from the routes, keeping reference to the base node 
        for routeFn in routes do
            routeFn (next,fail) inode |> ignore        

    interface INode<'T> with
        member __.Next with get () = Unchecked.defaultof<INode<'T>> and set _ = ()
        member __.Fail with get () = Unchecked.defaultof<INode<'T>> and set _ = ()
        member x.Apply (state:State<'T>) = 
            processPath fail inode (state)

let inline router (routes:((INode<'T> * INode<'T>) -> RNode<'T> -> RNode<'T>) list) =
    fun (next:INode<'T>,fail:INode<'T>) ->
        RouterNode<'T>(next,fail,routes) :> INode<'T>


let subRoute (path:string) (fns:((INode<'T> * INode<'T>) -> RNode<'T>->RNode<'T>) list) = 
    fun (itree:INode<'T> * INode<'T>) (parent:RNode<'T>) ->
        let child = RNode.ExtendPath parent path Empty
        for fn in fns do
            fn itree child |> ignore
        child


/// **Description**
///     Choose provides a list of options the app attempts in order listed, returning false in any pipeline will proceed to the next pipeline on the list 
/// **Parameters**
///   * `fns` - parameter of type `PipeLine<'T> list` 
///     *Pipelines required so if using a single Handler, use `pipeline` function to convert/wrap handler to pipeline*
///
/// **Output Type**
///   * `ChooseWrap<'T>` - A Temporary type wrapper used for composition binding
///
/// **Exceptions**
///
/// 
type PipelineList<'T> = PipeLine<'T> list
type ActionList<'T> = (State<'T> -> unit) list


// Wraps a Handler (Zapp) in a Pipeline function to allow binding in choose and other fixed seq type scenarios 
let pipeline (fn:State<'T> -> unit) =
    fun (next:INode<'T>,fail:INode<'T>) ->
        ChoiceNode(next,fail,fn) :> INode<'T>

type ChooseWrap<'T> =
| ChooseWrap of (INode<'T> * INode<'T> -> INode<'T>)

type ChooseBuilder() =
    member x.Delay(f) = f() 
    member x.YieldFrom(pipe:PipeLine<'T>) = ChooseWrap(pipe)

    member x.Yield(action:State<'T> -> unit) = ChooseWrap(fun (next:INode<'T>,fail:INode<'T>) -> ChoiceNode(next,fail,action) :> INode<'T> )

    member x.Combine(ChooseWrap(parentFn),ChooseWrap(childFn)) =
        ChooseWrap(fun (next:INode<'T>,fail:INode<'T>) ->
            let child = childFn(next,fail)
            parentFn(next,child)
        )

let choose = ChooseBuilder()

let choosePipe (fns:PipeLine<'T> list) =     
    ChooseWrap(fun (next:INode<'T>,fail:INode<'T>) ->
    let rec go (ls:PipeLine<'T> list) =
        match ls with
        | [] -> fail
        | h :: t ->
            h(next,go t)
    go fns)           

