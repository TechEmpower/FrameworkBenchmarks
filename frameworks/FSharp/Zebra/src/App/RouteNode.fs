module RouteNode
open State
open ExecNodes
open Parsers
open System.Collections.Generic
open System.Text
open System.Collections.Generic


type IRouteNode<'T> =
    abstract Parse : Range [] * State<'T> -> bool
    

let validFormats = set ['s';'i';'b';'f';'o'] // the PrintfFormat already valids but we check in pattern parse to be sure

// Helper Functions
////////////////////////

/// Tail Clip: clip end of 'str' string staring from int pos -> end
let inline (-|) (str:string) (from:int) = str.Substring(from,str.Length - from)
let commonPathIndex (str1:string) (idx1:int) (str2:string) =
    let rec go i j =
        if i < str1.Length && j < str2.Length then
            if str1.[i] = str2.[j]
            then go (i + 1) (j + 1)
            else j
        else j
    go idx1 0

let commonPath (str1:string) (str2:string) =
    let rec go i =
        if i < str1.Length && i < str2.Length then
            if str1.[i] = str2.[i]
            then go (i + 1)
            else i
        else i
    go 0

type PathMatch =
| SubMatch of int
| PathInToken
| TokenInPath
| ZeroToken
| ZeroMatch
| FullMatch

let private getPathMatch (path:string) (token:string) =
    if token.Length = 0 then ZeroToken
    else
        let cp = commonPath path token
        let tokenMatch = cp = token.Length
        let pathMatch = cp = path.Length
        if cp = 0 then ZeroMatch
        elif tokenMatch && pathMatch then FullMatch
        elif tokenMatch then TokenInPath
        elif pathMatch  then PathInToken
        else SubMatch cp

// New Methods filter
type METHODS =
| GET   = 0uy
| POST  = 1uy
| PUT   = 2uy
| DELETE = 3uy
| PATCH  = 4uy
| UNKNOWN = 5uy
let inline methodMatch (ctx:State<'T>,method:METHODS ) = 
        method =
            match ctx.HttpContext.Request.Method with
            | "GET"     -> METHODS.GET 
            | "POST"    -> METHODS.POST
            | "PUT"     -> METHODS.PUT
            | "DELETE"  -> METHODS.DELETE
            | "PATCH"   -> METHODS.PATCH
            | _         -> METHODS.UNKNOWN
///////

type RNode<'T>(token:string) =
    let mutable midFns : Cont<'T> list = []
    let mutable endFns : Cont<'T> list = []

    let addMidFn (mfn:Cont<'T>) = midFns <- mfn :: midFns |> List.sortBy (fun f -> f.Precedence)
    let addEndFn (efn:Cont<'T>) = endFns <- efn :: endFns |> List.sortBy (fun f -> f.Precedence)

    let mutable edges = Dictionary<char,RNode<'T>>()
    member __.Edges
        with get() = edges
        and set v = edges <- v
    member val Token = token with get,set
    member __.MidFns
        with get() = midFns
        and set v = midFns <- v
    member __.AddMidFn = addMidFn
    member __.EndFns
        with get()  = endFns
        and set v = endFns <- v
    member __.AddEndFn = addEndFn
    member __.EdgeCount
        with get () = edges.Count
    member __.GetEdgeKeys = edges.Keys
    member __.TryGetValue v = edges.TryGetValue v

    member val MethodFilters = HashSet<METHODS>() with get,set
    override x.ToString() =
        let sb = StringBuilder()
        x.ToString(0, sb)
        sb.ToString()

    member x.ToString (depth:int, sb:StringBuilder) =
            sb  .Append("(")
                .Append(x.Token)
                .Append(",{")
                .Append(sprintf "%A" midFns)
                .Append("|")
                .Append(sprintf "%A" endFns)
                .Append("},[")          |> ignore
            if x.Edges.Count = 0 then
                sb.Append("])\n")          |> ignore
            else
                sb.Append("\n")         |> ignore
                for kvp in x.Edges do
                    for _ in 0 .. depth do sb.Append("\t") |> ignore
                    sb  .Append(kvp.Key)
                        .Append(" => ") |> ignore
                    kvp.Value.ToString(depth + 1,sb)
                for _ in 0 .. depth do sb.Append("\t") |> ignore
                sb.Append("])\n")    |> ignore
                
    static member AddFn (node:RNode<'T>) fn =
        match fn with
        | Empty -> ()
        // Mid Functions
        | InitialMatch _
        | ApplyMatch _
        | MatchComplete _ -> node.MidFns <- fn :: node.MidFns |> List.sortBy (fun f -> f.Precedence)
        // End Functions
        | HandlerMap _
        | Complete   _    -> node.EndFns <- fn :: node.EndFns |> List.sortBy (fun f -> f.Precedence)

    static member Split (node:RNode<'T>) (pos:int) =
        // need to split existing node out
        let sedges = node.Edges //get ref to pass to split node
        let baseToken = node.Token.Substring(0,pos) //new start base token
        let childToken = (node.Token -| pos)
        let snode = RNode(childToken)
        node.Edges <- Dictionary<_,_>() //wipe edges from node before adding new edge
        node.Edges.Add(childToken.[0],snode)
        //node.Add childToken Empty // create split node
        node.Token <- baseToken
        snode.Edges <- sedges //pass old edges dictionary to split node
        //copy over existing functions
        snode.MidFns <- node.MidFns
        snode.EndFns <- node.EndFns
        //clear functions from existing node
        node.MidFns <- List.empty
        node.EndFns <- List.empty

    static member ExtendPath (node:RNode<'T>) (path:string) (rc:Cont<'T>) =
        if path = "" then
            RNode.AddFn node rc
            node
        else
            match node.TryGetValue path.[0] with
            | true, cnode ->
                RNode.AddPath cnode path rc // recursive path scan
            | false, _    ->
                let nnode = RNode(path)
                node.Edges.Add(path.[0], nnode)
                RNode.AddFn nnode rc
                nnode

    static member AddPath (node:RNode<'T>) (path:string) (rc:Cont<'T>) =

        //printfn "'%s' -> %s" path (node.ToString())

        match getPathMatch path node.Token with
        | ZeroToken ->
            // if node empty/root
            node.Token <- path
            RNode.AddFn node rc
            node
        | ZeroMatch ->
            failwith <| sprintf "path passed to node with non-matching start in error:%s -> %s\n\n%s\n" path node.Token (node.ToString())
        | FullMatch ->
            RNode.AddFn node rc
            node
        | PathInToken ->
            RNode.Split node (path.Length)
            RNode.AddFn node rc
            node
        | TokenInPath ->
            //path extends beyond this node
            let rem = path -| (node.Token.Length)
            match node.TryGetValue rem.[0] with
            | true, cnode ->
                RNode.AddPath cnode rem rc // recursive path scan
            | false, _    ->
                let nnode = RNode(rem)
                node.Edges.Add(rem.[0], nnode)
                RNode.AddFn nnode rc
                nnode
        | SubMatch (i) ->
            RNode.Split node (i)
            let rem = path -| i
            let nnode = RNode(rem)
            node.Edges.Add(rem.[0],nnode)
            RNode.AddFn nnode rc
            nnode

// Route Continuation Functions
////////////////////////////////
and Cont<'T> = 
| Empty
// MID
| InitialMatch of (int * char * (char []) * RNode<'T>) // (max requried range array,next match chars,next node)
| ApplyMatch of  (int * char * (char []) * RNode<'T>) // (range array position, format char, next match chars, next node)
| MatchComplete of METHODS * int * IRouteNode<'T>
// End
| HandlerMap of METHODS * INode<'T>
| Complete   of METHODS * IRouteNode<'T>
    member x.Precedence
        with get () =
            match x with
            | HandlerMap _      -> 0
            | InitialMatch _    -> 1            
            | ApplyMatch _      -> 2
            | MatchComplete _   -> 3             
            | Complete _        -> 4
            | Empty             -> 5

let private emptyMethodSet = HashSet<METHODS>()

let MethodOptimise (root:RNode<'T>) =

    let rec go(node:RNode<'T>,parent:HashSet<METHODS>) =
        for kvp in node.Edges do
            if kvp.Value.MethodFilters = parent then
                kvp.Value.MethodFilters <- emptyMethodSet // clear the hashset
                go(kvp.Value,parent)
            else
                go(kvp.Value,kvp.Value.MethodFilters)
    go(root,root.MethodFilters)
