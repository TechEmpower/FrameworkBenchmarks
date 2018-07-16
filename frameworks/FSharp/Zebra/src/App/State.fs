module State

open System.Threading.Tasks
open System
open System.Runtime.CompilerServices
open System.Threading
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Primitives
open StreamBuffer
open System.IO
open EncodeHelper

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
type FinishStateAwaiter(methodBuilder:AsyncTaskMethodBuilder,ctx:HttpContext) =
    interface IAsyncStateMachine with
        member __.MoveNext() =         
            //ctx.Response.Body.Flush()
            methodBuilder.SetResult()
            
            ctx.Abort()
        member __.SetStateMachine sm = methodBuilder.SetStateMachine sm    

[<Struct>]
type FinishStateMemoryAwaiter(methodBuilder:AsyncTaskMethodBuilder,ms:MemoryStream) =
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

type INode<'T> =
    abstract member Next  : INode<'T> with get, set
    abstract member Fail  : INode<'T> with get, set
    abstract member Apply : State<'T> -> unit

and State<'T>(hctx: HttpContext, deps: 'T,amb: AsyncTaskMethodBuilder) =
        //let buffer = MemoryStreamCache.Get()
        
        member val MethodBuilder : AsyncTaskMethodBuilder = amb with get,set
        member val HttpContext              = hctx with get,set
        member val Dependencies             = deps with get // alternative dependency injection system
        member val DNode : INode<'T>        = Unchecked.defaultof<INode<'T>> with get, set // <<<TEMP

        member val PathPosition             = 0 with get, set // pos starts 0 and mutates via routers

        member val Disposables              = List.empty<IDisposable> with get, set
        //member __.Buffer with get () = buffer
        member val ContentType = ContentType.Text with get, set

        member inline x.Next   () = x.DNode.Next.Apply x
        member inline x.Fail   () = x.DNode.Next.Apply x
                   
        // Computaion Expresssion Members
        /////////////////////////////////
        member inline x.Zero() =
            // Zero presumes a true, failing must be explicit, progressing is implicit 
            x.DNode.Next.Apply x
        member x.Return(_:unit) = x.DNode.Next.Apply x
        member x.Return(result: bool) = 
            if result then
                x.DNode.Next.Apply x
            else
                x.DNode.Fail.Apply x

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
               
        member x.SetComplete() =            
            // if x.Buffer.Length > 0L then
            //     x.HttpContext.Response.Headers.["Content-Length"] <- StringValues(x.Buffer.Length.ToString())
            //     x.HttpContext.Response.StatusCode <- 200
            //     x.HttpContext.Response.Headers.["Content-Encoding"] <- StringValues("identity")
            //     x.HttpContext.Response.Headers.["Content-Type"] <- 
            //         match x.ContentType with
            //         | ContentType.Text -> "text/plain" 
            //         | ContentType.Json -> "application/json"
            //         | _ -> "text/plain"
            //         |> StringValues

            //     x.Buffer.Seek(0L,SeekOrigin.Begin) |> ignore

            //     let t = x.Buffer.CopyToAsync(x.HttpContext.Response.Body)
            //     let awt = t.GetAwaiter()
            //     let mutable awtref = awt
            //     let mutable smref = FinishStateAwaiter(x.MethodBuilder,x.Buffer)
            //     x.MethodBuilder.AwaitUnsafeOnCompleted(&awtref,&smref)

            // else

            //     x.HttpContext.Response.Headers.["Content-Length"] <- StringValues("0")
            //     x.HttpContext.Response.StatusCode <- 404
            //     x.HttpContext.Response.Body.Flush()
            //     x.MethodBuilder.SetResult()

            // at end of pipeline when buffer written, we can start disposing waiting for Async copy to finish                         
            for disp in x.Disposables do disp.Dispose()            

        
        // Custom Ops
        /////////////////////////////
         
        
        // Text
        member inline x.Text(text: string ) =
            let ctx = x.HttpContext
            x.HttpContext <- Unchecked.defaultof<HttpContext>
            ctx.Response.Headers.["Content-Length"] <- StringValues(text.Length.ToString())            
            ctx.Response.Headers.["Content-Type"] <- StringValues "text/plain"
            ctx.Response.Headers.["Content-Encoding"] <- StringValues "identity"

            ctx.Response.StatusCode <- 200
            let t = ctx.Response.WriteAsync(text)
            let awt = t.GetAwaiter()
            let mutable awtref = awt
            let mutable smref = FinishStateAwaiter(x.MethodBuilder,ctx)
            x.MethodBuilder.AwaitUnsafeOnCompleted(&awtref,&smref)
                        
            for disp in x.Disposables do disp.Dispose()

        // [<CustomOperation("text",MaintainsVariableSpaceUsingBind=true)>] //,MaintainsVariableSpaceUsingBind = true)>]
        
        // /// **Description**
        // ///   Writes text to the state buffer that will be flushed at the end  
        // /// **Parameters**
        // ///   * `
        // ///   * `text` - parameter of type `string`

        // member inline x.Text<'a>(_:unit, text: string ) = x.Text(text)


        // Json
        member inline x.Json< ^a>(value: ^a ) =
            let buffer = MemoryStreamCache.Get()
            x.HttpContext.Response.Headers.["Content-Type"] <- StringValues "application/json"
            
            
            Utf8Json.JsonSerializer.Serialize< ^a>(buffer,value)
            x.HttpContext.Response.Headers.["Content-Length"] <- StringValues(buffer.Length.ToString())            
            buffer.Seek(0L,SeekOrigin.Begin) |> ignore
            let t = buffer.CopyToAsync(x.HttpContext.Response.Body)
            let awt = t.GetAwaiter()
            let mutable awtref = awt
            let mutable smref = FinishStateMemoryAwaiter(x.MethodBuilder,buffer)
            x.MethodBuilder.AwaitUnsafeOnCompleted(&awtref,&smref)
            
            for disp in x.Disposables do disp.Dispose()
        
        // [<CustomOperation("json",MaintainsVariableSpaceUsingBind=true)>]//,MaintainsVariableSpaceUsingBind = true)>]
        // member inline x.Json< ^a>(_:unit, value: ^a ) = x.Json(value)


        // SetHeader
        member inline x.SetHeader(header: string, value:string ) =
            x.HttpContext.Response.Headers.[header] <- StringValues(value)    

        // [<CustomOperation("setHeader",MaintainsVariableSpaceUsingBind=true)>]//,MaintainsVariableSpaceUsingBind = true)>]
        // member inline x.SetHeader(_:unit, header: string, value:string ) = x.SetHeader(header,value)
    
        member inline x.SetStatus(value:int ) =
            x.HttpContext.Response.StatusCode <- value  

        // [<CustomOperation("status",MaintainsVariableSpaceUsingBind=true)>]//,MaintainsVariableSpaceUsingBind = true)>]
        // member inline x.SetStatus(_:unit, value:int ) = x.SetStatus(value)


and Zapp<'T> = State<'T> -> unit
and PipeLine<'T> = (INode<'T> * INode<'T>) -> INode<'T>


