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
open System.Net.Mime

[<Struct>]
type GenericStateAwaiter<'T>(awaiter:TaskAwaiter<'T> ,continuation:'T -> unit,methodBuilder:AsyncTaskMethodBuilder) =
    interface IAsyncStateMachine with
        member __.MoveNext() =
                continuation ( awaiter.GetResult() )  // runs cont, will update awaiter & continuation
        member __.SetStateMachine (sm) = methodBuilder.SetStateMachine sm 

[<Struct>]
type PlainStateAwaiter(continuation:unit -> unit,methodBuilder:AsyncTaskMethodBuilder) =
    interface IAsyncStateMachine with
        member __.MoveNext() =
                continuation ()  // runs cont, will update awaiter & continuation
        member __.SetStateMachine sm = methodBuilder.SetStateMachine sm

[<Struct>]
type FinishStateAwaiter(methodBuilder:AsyncTaskMethodBuilder,ms:MemoryStream) =
    interface IAsyncStateMachine with
        member __.MoveNext() =         
            MemoryStreamCache.Release ms
            methodBuilder.SetResult()
        member __.SetStateMachine sm = methodBuilder.SetStateMachine sm  


////////////////
// nodes are embedded in list so on each node load there is no additional calls to fucntions
type ContentType =
| Text = 0uy
| Json = 1uy
| Xml = 2uy
| Custom = 3uy


type INode<'T> =
    abstract member Next  : INode<'T> with get, set
    abstract member Fail  : INode<'T> with get, set
    abstract member Apply : State<'T> -> unit

and State<'T>(hctx: HttpContext, deps: 'T,amb: AsyncTaskMethodBuilder) =
        let buffer = MemoryStreamCache.Get()
        
        member val MethodBuilder : AsyncTaskMethodBuilder = amb with get
        member val HttpContext              = hctx with get,set
        member val Dependencies             = deps with get // alternative dependency injection system
        member val DNode : INode<'T>        = Unchecked.defaultof<INode<'T>> with get, set // <<<TEMP

        member val PathPosition             = 0 with get, set // pos starts 0 and mutates via routers

        member val Disposables              = List.empty<IDisposable> with get, set
        member __.Buffer with get () = buffer
        member val ContentType = ContentType.Text with get, set

        member inline x.Next   () = x.DNode.Next.Apply x
        member inline x.Fail   () = x.DNode.Next.Apply x
                   
        // Computaion Expresssion Members
        /////////////////////////////////
        member inline x.Run(_:unit) = () // runs last on customOperations, runs after first bind

        member inline x.Run(n:INode<'T>) = // runs last on customOperations, runs after first bind
            n.Apply x


        member inline x.Zero() =
            // Zero presumes a true, failing must be explicit, progressing is implicit 
            x.DNode.Next.Apply x

        member inline x.Return(_:unit) = x.DNode.Next 
        member inline x.Return(result: bool) = // runs after last bind
            if result then
                x.DNode.Next.Apply x
            else
                x.DNode.Fail.Apply x
        member inline x.Return(a) = () // passthrough bound values for now

        member inline x.Bind<'a,'b>(n:INode<'T>,b:unit -> unit) = b () // input is output of last custom op

        member inline x.Bind(task:Task, continuation : unit -> unit) : unit =
            let awt : TaskAwaiter = task.GetAwaiter()
            if awt.IsCompleted then
                continuation ()
            else
                let mutable awtref = awt
                let mutable smref = PlainStateAwaiter(continuation,x.MethodBuilder)
                x.MethodBuilder.AwaitUnsafeOnCompleted(&awtref,&smref)

        member inline x.Bind(configurableTaskLike:Task< ^inp>, continuation : ^inp -> unit) : unit =
            let awt : TaskAwaiter< ^inp> = configurableTaskLike.GetAwaiter() 
            if awt.IsCompleted then
                continuation (awt.GetResult())
            else
                let mutable awtref = awt
                let mutable smref =  GenericStateAwaiter< ^inp>(awt,continuation,x.MethodBuilder)
                x.MethodBuilder.AwaitUnsafeOnCompleted(&awtref,&smref)          

        member inline x.Using(disp : #IDisposable, continuation : #IDisposable -> unit) =
            x.Disposables <- disp :> IDisposable :: x.Disposables
            continuation disp
               
        member inline x.SetComplete() =
            let inline contType (v:string) = x.HttpContext.Response.Headers.["Content-Type"] <- StringValues v
            
            if x.Buffer.Length > 0L then
                x.HttpContext.Response.Headers.["Content-Length"] <- StringValues(x.Buffer.Length.ToString())
                x.HttpContext.Response.StatusCode <- 200
                match x.ContentType with
                | ContentType.Custom -> ()
                | ContentType.Text -> contType "text/plain" 
                | ContentType.Json -> contType "application/json"
                | ContentType.Xml  -> contType "application/xml"
                | _                -> contType "text/plain"

                x.Buffer.Seek(0L,SeekOrigin.Begin) |> ignore

                let t = x.Buffer.CopyToAsync(x.HttpContext.Response.Body)
                let awt = t.GetAwaiter()
                let mutable awtref = awt
                let mutable smref = FinishStateAwaiter(x.MethodBuilder,x.Buffer)
                x.MethodBuilder.AwaitUnsafeOnCompleted(&awtref,&smref)

            else                
                x.HttpContext.Response.Headers.["Content-Length"] <- StringValues("0")
                x.HttpContext.Response.StatusCode <- 404
                x.MethodBuilder.SetResult()

            // at end of pipeline when buffer written, we can start disposing waiting for Async copy to finish                         
            for disp in x.Disposables do disp.Dispose()   // may need to be pushed out to FinishStateAwaiter
        
        // Custom Ops
        /////////////////////////////
        

        // Text
        ////////
        member inline x.Text(text: string ) = StreamWrite(text,x.Buffer) ;    

        [<CustomOperation("text",MaintainsVariableSpaceUsingBind=true)>] //,MaintainsVariableSpaceUsingBind = true)>]
        
        /// **Description**
        ///   Writes text to the state buffer that will be flushed at the end  
        /// **Parameters**
        ///   * `
        ///   * `text` - parameter of type `string`

        member inline x.Text<'a>(n:INode<'T>, text: string ) =  x.Text(text); n

        // Json
        //////////
        
        member inline x.Json< ^a>(value: ^a ) =
            Utf8Json.JsonSerializer.Serialize< ^a>(x.Buffer,value)
            x.ContentType <- ContentType.Json
        [<CustomOperation("json",MaintainsVariableSpaceUsingBind=true)>]//,MaintainsVariableSpaceUsingBind = true)>]
        member inline x.Json< ^a>(n:INode<'T>, value: ^a ) =  x.Json< ^a>(value: ^a); n


        // XML
        /////////
        member inline x.Xml< ^a>(value: ^a ) =
            let ser = XmlSerializer(typeof< ^a>).Serialize(x.Buffer,value)   // <<< should find better serialiser
            x.ContentType <- ContentType.Xml

        [<CustomOperation("xml",MaintainsVariableSpaceUsingBind=true)>]//,MaintainsVariableSpaceUsingBind = true)>]
        member inline x.Xml< ^a>(n:INode<'T>, value: ^a ) = x.Xml< ^a>(value: ^a) ; n
        
        // setHeader
        ////////////
        member inline x.SetHeader(header: string, value:string ) =
            x.HttpContext.Response.Headers.[header] <- StringValues(value)  

        [<CustomOperation("setHeader",MaintainsVariableSpaceUsingBind=true)>]//,MaintainsVariableSpaceUsingBind = true)>]
        member inline x.SetHeader(n:INode<'T>, header: string, value:string ) = x.SetHeader(header, value) ; n

        // status code
        ///////////////
        member inline x.SetStatus(value:int ) =
            x.HttpContext.Response.StatusCode <- value

        [<CustomOperation("status",MaintainsVariableSpaceUsingBind=true)>]//,MaintainsVariableSpaceUsingBind = true)>]
        member inline x.SetStatus(n:INode<'T>, value:int ) = x.SetStatus(value) ; n

        // content_type
        ////////////////////
        member inline x.SetContentType(value:string ) =
            x.ContentType <- ContentType.Custom
            x.HttpContext.Response.Headers.["Content-Type"] <- StringValues value

        
        [<CustomOperation("content_type",MaintainsVariableSpaceUsingBind=true)>]//,MaintainsVariableSpaceUsingBind = true)>]
        member inline x.SetContentType(_:unit, value:string ) = x.SetContentType(value)



and Zapp<'T> = State<'T> -> unit
and PipeLine<'T> = (INode<'T> * INode<'T>) -> INode<'T>


