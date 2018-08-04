[<AutoOpenAttribute>]
module Middleware

open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder

open State
open Router
open ExecNodes

open System.Threading.Tasks

type ZebraMiddleware<'T>(
                        next          : RequestDelegate,
                        Dependencies  : 'T,
                        failAction    : State<'T> -> unit,
                        AppBuilder    : PipeLine<'T>
                        ) =
    let finishNode = FinishNode() :> INode<'T>
    let failNode = FailNode<'T>(failAction,finishNode)
    let appNode = AppBuilder(finishNode,failNode) // build App node
    
    //do GC.Collect()
    
    member __.Invoke (ctx : HttpContext) = 
        
        let tcs = TaskCompletionSource()
                
        let mutable state = State<'T>(ctx,Dependencies,tcs)
        appNode.Apply state

        tcs.Task


type ZebraSimpleMiddleware(
                        next          : RequestDelegate,
                        App    : State<unit> -> unit
                        ) =
    
    //do GC.Collect()
    
    member __.Invoke (ctx : HttpContext) = 
        
        let tcs  = TaskCompletionSource()
                
        let mutable state = State<unit>(ctx,(),tcs)
        App state

        tcs.Task

type IApplicationBuilder with
    member x.UseZebraMiddleware<'T>(dependencies:'T,fallback:Zapp<'T>,app:PipeLine<'T>) = 
        x.UseMiddleware<ZebraMiddleware<'T>> [|box dependencies;box fallback;box app|]

    member x.UseZebraSimpleMiddleware(app:State<'T> -> unit) = 
         x.UseMiddleware<ZebraSimpleMiddleware> [|box app|]