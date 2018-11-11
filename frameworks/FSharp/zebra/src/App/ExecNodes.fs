module ExecNodes
open State

/// Node Types
/// //////////////////
                     
type ChoiceNode<'T>(inext:INode<'T>,ifail:INode<'T>,current:Zapp<'T>) =
    let mutable next = inext
    let mutable fail = ifail

    interface INode<'T> with
        member __.Next with get () = next and set v = next <- v
        member __.Fail with get () = fail and set v = fail <- v
        member x.Apply (state:State<'T>) =
            state.DNode <- x // inject choices
            current state  // run next/fail test    

// the finish node sets result on the state-machine task  
and FinishNode<'T>() =
    interface INode<'T> with
        member __.Apply (state:State<'T>) = 
            state.SetComplete()
        member __.Next with get () = Unchecked.defaultof<INode<'T>> and set _ = ()
        member __.Fail with get () = Unchecked.defaultof<INode<'T>> and set _ = ()

// the fail node will me a single instance put in all end fails
and FailNode<'T>(fail:State<'T> -> unit,finishNode:INode<'T>) =
    interface INode<'T> with
        member x.Apply (state:State<'T>) = 
            // if state.Buffer.Length = 0L then
                state.DNode <- x
                fail state
            // else
                // finishNode.Apply state

        member __.Next with get () = finishNode and set _ = ()
        member __.Fail with get () = finishNode and set _ = ()