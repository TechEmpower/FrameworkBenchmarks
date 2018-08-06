module State

open System.Threading.Tasks
open System
open System.Runtime.CompilerServices
open System.Threading
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Primitives
open System.Xml.Serialization
open StreamBuffer
open System.IO
open EncodeHelper

open System.Text
open System
open TemplateViewEngine   

[<Struct>]
type GenericStateAwaiter<'T>(awaiter:TaskAwaiter<'T> ,continuation:'T -> unit) =
    member x.Apply() = continuation ( awaiter.GetResult() ) 

[<Struct>]
type PlainStateAwaiter(continuation:unit -> unit) =
    member x.MoveNext() = continuation () 


////////////////
// nodes are embedded in list so on each node load there is no additional calls to fucntions

type MachineState =
| Start = 0uy
| BindUnit = 1uy
| BindGen = 2uy
| Complete = 3uy
| CompleteRelease = 4uy 

[<Struct>]
type BindType =
| None
| Text of text:string
| Stream of stream:MemoryStream
| Bytes of bytes:byte []

type INode<'T> =
    abstract member Next  : INode<'T> with get, set
    abstract member Fail  : INode<'T> with get, set
    abstract member Apply : State<'T> -> unit

and State<'T>(hctx: HttpContext, deps: 'T, amb: TaskCompletionSource<unit>) =

        let mutable buffer = Unchecked.defaultof<MemoryStream>
        let mutable bindUnit = Unchecked.defaultof<unit -> unit>
        let mutable bindGen = Unchecked.defaultof<GenericStateAwaiter<obj>>
        let mutable currentState = MachineState.Start
        member x.BindUnit with get() = bindUnit and set v = bindUnit <- v
        member x.BindGen with get() = bindGen and set v = bindGen <- v
        member val BindType = BindType.None with get,set
        member x.CurrentState with get() = currentState and set v = currentState <- v
        member x.Continue : Action = Action (fun _ ->
            match currentState with
            | MachineState.Start    -> ()
            | MachineState.BindGen  ->  bindGen.Apply()
            | MachineState.BindUnit ->  bindUnit ()
            | MachineState.CompleteRelease ->
                amb.SetResult()
                MemoryStreamCache.Release buffer
            | MachineState.Complete
            | _ -> amb.SetResult()
        )       
        member val HttpContext        : HttpContext = hctx with get
        member val Dependencies : 'T = deps with get,set
        member val DNode = Unchecked.defaultof<INode<'T>> with get,set
        member val PathPosition : int = 0 with get, set
        member val Disposables : IDisposable list = [] with get, set 
                   
        // Computaion Expresssion Members
        /////////////////////////////////
        member inline x.Run(_:unit) = ()  // runs after first async bind

        member inline x.Run(n:BindType) = // runs last on customOperations,
            match n with
            | BindType.None -> ()
            | BindType.Text _ 
            | BindType.Stream _ 
            | BindType.Bytes _ -> x.BindType <- n
            x.DNode.Next.Apply x

        member inline x.Zero() =
            // Zero presumes a true, failing must be explicit, progressing is implicit
            x.DNode.Next.Apply (x)

        member inline x.Return(_:unit) = x.BindType 
        
        member inline x.Return(result: bool) = // runs after last bind
            if result then
                x.DNode.Next.Apply (x)
            else
                x.DNode.Fail.Apply (x)                
            
        member inline x.Bind(bt:BindType,b:unit -> unit) =
            x.BindType <- bt
            b ()// input is output of last custom op

        member inline x.Bind(task:Task,continuation : unit -> unit) : unit =
            let mutable awt : TaskAwaiter = task.GetAwaiter()
            if awt.IsCompleted then
                continuation ()
            else
                x.BindUnit <- continuation
                x.CurrentState <- MachineState.BindUnit
                awt.UnsafeOnCompleted x.Continue

        member inline x.Bind< ^inp>(taskGen:Task< ^inp>,continuation : ^inp -> unit) : unit =
            let mutable awt : TaskAwaiter< ^inp> = taskGen.GetAwaiter() 
            if awt.IsCompleted then
                continuation (awt.GetResult())
            else
                let mutable smref = GenericStateAwaiter< ^inp>(awt,continuation)
                let mutable cast = Unsafe.As<GenericStateAwaiter< ^inp>,GenericStateAwaiter<obj>>(&smref)
                x.BindGen <- cast
                x.CurrentState <- MachineState.BindGen
                awt.UnsafeOnCompleted x.Continue  

        member inline x.Using(disp : #IDisposable, continuation : #IDisposable -> unit) =
            x.Disposables <- disp :> IDisposable :: x.Disposables
            continuation disp

        member x.SetComplete() =
            match x.BindType with
            | BindType.None -> amb.SetResult()
            | BindType.Text v ->
                let bytes = Encoding.UTF8.GetBytes(v)
                x.HttpContext.Response.Headers.["Content-Length"] <- StringValues(bytes.Length.ToString())
                let t = x.HttpContext.Response.Body.WriteAsync(bytes,0,bytes.Length)
                x.CurrentState <- MachineState.Complete
                t.GetAwaiter().UnsafeOnCompleted x.Continue
                
            | BindType.Stream ms ->
                buffer <- ms
                x.HttpContext.Response.Headers.["Content-Length"] <- StringValues(ms.Length.ToString())
                buffer.Seek(0L,SeekOrigin.Begin) |> ignore
                let t = ms.CopyToAsync(x.HttpContext.Response.Body)
                x.CurrentState <- MachineState.CompleteRelease
                t.GetAwaiter().UnsafeOnCompleted x.Continue

            | BindType.Bytes bytes ->
                x.HttpContext.Response.Headers.["Content-Length"] <- StringValues(bytes.Length.ToString())
                let t = x.HttpContext.Response.Body.WriteAsync(bytes,0,bytes.Length)
                x.CurrentState <- MachineState.Complete
                t.GetAwaiter().UnsafeOnCompleted x.Continue

            // at end of pipeline when buffer written, we can start disposing waiting for Async copy to finish                         
            for disp in x.Disposables do disp.Dispose()   // may need to be pushed out to FinishStateAwaiter
        
        // Custom Ops
        /////////////////////////////
        
        
        // Text
        ////////

        [<CustomOperation("text",MaintainsVariableSpaceUsingBind=true)>] //,MaintainsVariableSpaceUsingBind = true)>]
        
        /// **Description**
        ///   Writes text to the state buffer that will be flushed at the end  
        /// **Parameters**
        ///   * `
        ///   * `text` - parameter of type `string`
        member inline x.Text(bt:BindType, text: string ) =
            match bt with
            | BindType.None -> 
                x.HttpContext.Response.Headers.["Content-Type"] <- StringValues "text/plain"
                BindType.Text(text)
            | BindType.Text v ->
                let ms = MemoryStreamCache.Get()
                StreamWrite(v,ms)
                StreamWrite(text,ms)
                BindType.Stream ms
            | BindType.Stream ms ->
                StreamWrite(text,ms)
                bt
            | BindType.Bytes _ -> failwith "Error : passed json bytes into a text pipeline!?!"

        member inline x.Text(text: string ) = x.Text(x.BindType,text)        

        // Json
        //////////
        
        [<CustomOperation("json",MaintainsVariableSpaceUsingBind=true)>]//,MaintainsVariableSpaceUsingBind = true)>]
        member inline x.Json< ^a>(n:BindType, value: ^a ) =
            match n with
            | BindType.None -> 
                x.HttpContext.Response.Headers.["Content-Type"] <- StringValues "application/json"
                BindType.Bytes (Utf8Json.JsonSerializer.Serialize< ^a>(value))
            | BindType.Text _ -> failwith "Error : passed text into json funtion"
            | BindType.Stream ms -> Utf8Json.JsonSerializer.Serialize< ^a>(ms,value) ; n
            | BindType.Bytes bytes ->
                let ms = MemoryStreamCache.Get()
                ms.Write(bytes,0,bytes.Length)
                Utf8Json.JsonSerializer.Serialize< ^a>(ms,value)
                BindType.Stream ms

        member inline x.Json< ^a>(value: ^a ) = x.Json< ^a>(x.BindType, value) 

         
       
        [<CustomOperation("render",MaintainsVariableSpaceUsingBind=true)>]//,MaintainsVariableSpaceUsingBind = true)>]
        member inline x.Render< ^a>(n:BindType, value: ^a, compiledNodes :CompiledNode< ^a> [] ) =
            match n with
            | BindType.None -> 
                x.HttpContext.Response.Headers.["Content-Type"] <- StringValues "text/html;charset=utf-8"
                let ms = MemoryStreamCache.Get()
                renderHtmlDocument value compiledNodes ms
                BindType.Stream ms
            | _ -> failwith "render can be the only writer in a pipeline" 

        member inline x.Render< ^a>(value: ^a,compiledNodes :CompiledNode< ^a> []) =
            x.HttpContext.Response.Headers.["Content-Type"] <- StringValues "text/html;charset=utf-8"
            let ms = MemoryStreamCache.Get()
            renderHtmlDocument value compiledNodes ms
            x.BindType <- BindType.Stream ms

        // setHeader
        ////////////
        member inline x.SetHeader(header: string, value:string ) =
            x.HttpContext.Response.Headers.[header] <- StringValues(value)  

        [<CustomOperation("setHeader",MaintainsVariableSpaceUsingBind=true)>]//,MaintainsVariableSpaceUsingBind = true)>]
        member inline x.SetHeader(n:BindType, header: string, value:string ) = x.SetHeader(header, value) ; n

        // status code
        ///////////////
        member inline x.SetStatus(value:int ) =
            x.HttpContext.Response.StatusCode <- value

        [<CustomOperation("status",MaintainsVariableSpaceUsingBind=true)>]//,MaintainsVariableSpaceUsingBind = true)>]
        member inline x.SetStatus(n:BindType, value:int ) = x.SetStatus(value) ; n

        // content_type
        ////////////////////
        member inline x.SetContentType(value:string ) =
            x.HttpContext.Response.Headers.["Content-Type"] <- StringValues value

        
        [<CustomOperation("contentType",MaintainsVariableSpaceUsingBind=true)>]//,MaintainsVariableSpaceUsingBind = true)>]
        member inline x.SetContentType(n:BindType, value:string ) = x.SetContentType(value) ; n

and Zapp<'T> = State<'T> -> unit
and PipeLine<'T> = (INode<'T> * INode<'T>) -> INode<'T>


