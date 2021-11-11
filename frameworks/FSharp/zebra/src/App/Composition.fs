[<AutoOpen>]
module Composition
open State
open ExecNodes
open RouteNode
open Parsers
open Router

//////////////////////////////////////
/// Composisiotn
let inline composeActAct (handler1:Zapp<'T>) (handler2:Zapp<'T>) : PipeLine<'T> =
    fun (next:INode<'T>,fail:INode<'T>) -> // new nodeTree Fn
        let child =  ChoiceNode<'T>(next,fail,handler2)      // pushing initial next/fail down pipeline
        let parent = ChoiceNode<'T>(child,fail,handler1)
        parent :> INode<'T>

let inline composeFnAct (parentFn:PipeLine<'T>) (handler:Zapp<'T>) : PipeLine<'T> =
    fun (next:INode<'T>,fail:INode<'T>) -> // new nodeTree Fn
        let child = ChoiceNode<'T>(next,fail,handler)      // pushing initial next/fail down pipeline
        let parent = parentFn (child,fail)
        parent

let inline composeActFn (parentHandler:Zapp<'T>) (childFn:PipeLine<'T>) : PipeLine<'T> =
    fun (next:INode<'T>,fail:INode<'T>) -> // new nodeTree Fn
        let child = childFn(next,fail)      // pushing initial next/fail down pipeline
        let parent = ChoiceNode<'T>(child,fail,parentHandler)
        parent :> INode<'T>

let inline composeFnFn (parentFn:PipeLine<'T>) (childFn:PipeLine<'T>) : PipeLine<'T> =
    fun (next:INode<'T>,fail:INode<'T>) -> // new nodeTree Fn
        let child = childFn(next,fail)      // pushing initial next/fail down pipeline
        let parent = parentFn(child,fail)
        parent

let inline composeFnChoose (parentFn:PipeLine<'T>) (ChooseWrap(childFn):ChooseWrap<'T>) : PipeLine<'T> =
    fun (next:INode<'T>,fail:INode<'T>) -> // new nodeTree Fn
        let child = childFn(next,fail)
        let parent = parentFn(child,fail)
        parent

type RouteBuilder<'T> = INode<'T> * INode<'T> -> RNode<'T> -> RNode<'T>

let inline composeRouter (routeFn:RouteBuilder<'T>) (childFn:Zapp<'T>) : RouteBuilder<'T> =
    fun (next:INode<'T>,fail:INode<'T>) ->
        let child = ChoiceNode<'T>(next,fail,childFn)      // pushing initial next/fail down pipeline
        let parent = routeFn(child,fail)
        parent

let inline composeRoutePipe (routeFn:RouteBuilder<'T>) (childFn:PipeLine<'T>) : RouteBuilder<'T> =
    fun (next:INode<'T>,fail:INode<'T>) ->
        let child = childFn(next,fail)      // pushing initial next/fail down pipeline
        let parent = routeFn(child,fail)
        parent

type PipeRoute<'T> = INode<'T> * INode<'T> -> IRouteNode<'T>     
let inline composeRoute (pipeRoute: PipeRoute<'T>) (childFn:Zapp<'T>) =
    fun (next:INode<'T>,fail:INode<'T>) ->
        let child = ChoiceNode<'T>(next,fail,childFn)
        pipeRoute(child,fail)

let inline composeBase (a:RouteBase<'T>) (b:Zapp<'T>) =
    fun (next:INode<'T>,fail:INode<'T>) ->
        let child = ChoiceNode<'T>(next,fail,b)
        route a.Method a.Pattern (child,fail)

let inline composeRouteChoose (a:RouteBase<'T>) (ChooseWrap(b):ChooseWrap<'T>) =
    fun (next:INode<'T>,fail:INode<'T>) ->
        let child = b(next,fail)
        route a.Method a.Pattern (child,fail)


let inline composeBasePipe (a:RouteBase<'T>) (b:PipeLine<'T>) =
    fun (next:INode<'T>,fail:INode<'T>) ->
        let child = b(next,fail)
        route a.Method a.Pattern (child,fail)    

let inline composeBase1 (a:RouteBase<'T,'a>) (b:'a -> Zapp<'T>) =
    fun (next:INode<'T>,fail:INode<'T>) ->
        let p1 = Parse $ Unchecked.defaultof<'a>
        let rn = RouteNode1(next,fail,b,p1) :> IRouteNode<'T>
        routef a.Method a.Pattern rn 1

let inline composeBase2 (a:RouteBase<'T,'a,'b>) (b:'a -> 'b -> Zapp<'T>) =
    fun (next:INode<'T>,fail:INode<'T>) ->
        let p1 = Parse $ Unchecked.defaultof<'a>
        let p2 = Parse $ Unchecked.defaultof<'b>
        let rn = RouteNode2(next,fail,b,p1,p2) :> IRouteNode<'T>
        routef a.Method a.Pattern rn 2

let inline composeBase3 (a:RouteBase<'T,'a,'b,'c>) (b:'a -> 'b -> 'c -> Zapp<'T>) =
    fun (next:INode<'T>,fail:INode<'T>) ->
        let p1 = Parse $ Unchecked.defaultof<'a>
        let p2 = Parse $ Unchecked.defaultof<'b>
        let p3 = Parse $ Unchecked.defaultof<'c>
        let rn = RouteNode3(next,fail,b,p1,p2,p3) :> IRouteNode<'T>
        routef a.Method a.Pattern rn 3      

type ComposeExtension = ComposeExtension with

    // initial binding
    static member inline (?<-) (ComposeExtension, a:Zapp<'T> , b:Zapp<'T>) = composeActAct a b
    static member inline (?<-) (ComposeExtension, a:PipeLine<'T> , b:Zapp<'T>) = composeFnAct a b
    static member inline (?<-) (ComposeExtension, a:PipeLine<'T> , b:PipeLine<'T>) = composeFnFn a b
    static member inline (?<-) (ComposeExtension, a:PipeLine<'T> , b:ChooseWrap<'T>) = composeFnChoose a b
    static member inline (?<-) (ComposeExtension, a:RouteBuilder<'T> , b:Zapp<'T>) = composeRouter a b
    //static member inline (?<-) (ComposeExtension, a:RouteBuilder<'T> , b:PipeLine<'T>) = composeRoutePipe a b
    static member inline (?<-) (ComposeExtension, a:PipeRoute<'T> , b:Zapp<'T>) = composeRoute a b
    static member inline (?<-) (ComposeExtension, a:RouteBase<'T> , b:Zapp<'T>) = composeBase a b
    //static member inline (?<-) (ComposeExtension, a:RouteBase<'T> , b:PipeLine<'T>) = composeBasePipe a b
    static member inline (?<-) (ComposeExtension, a:RouteBase<'T> , b:ChooseWrap<'T>) = composeRouteChoose a b
    static member inline (?<-) (ComposeExtension, a:RouteBase<'T,'a> , b:'a -> Zapp<'T>) = composeBase1 a b
    static member inline (?<-) (ComposeExtension, a:RouteBase<'T,'a,'b> , b:'a -> 'b -> Zapp<'T>) = composeBase2 a b
    static member inline (?<-) (ComposeExtension, a:RouteBase<'T,'a,'b,'c> , b:'a -> 'b -> 'c -> Zapp<'T>) = composeBase3 a b
    
    //static member inline (?<-) (ComposeExtension, a:Action<'T> , b:PipeLine<'T>) = composeActFn a b
    
        
/// **Action Binder** : dynamically binds different kinds of actions/choices together 
/// eg:
///  (a:Zapp<'T> , b:Zapp<'T>) 
///  (a:PipeLine<'T> , b:Zapp<'T>) 
///  (a:PipeLine<'T> , b:PipeLine<'T>) 
let inline (=>) a b = (?<-) ComposeExtension a b