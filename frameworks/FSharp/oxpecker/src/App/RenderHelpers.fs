module RenderHelpers

    open System.Text
    open App
    open Oxpecker.ViewEngine

    type HeadAndTail =
        {
            Head: string
            Tail: string
        }

    [<Struct>]
    type CombinedElement(ht: HeadAndTail, fortunesData: ResizeArray<Fortune>) =
        interface HtmlElement with
            member this.Render(sb) =
                sb.Append(ht.Head) |> ignore
                for fortune in fortunesData do
                    (tr() {
                        td() { raw <| string fortune.id }
                        td() { fortune.message }
                    }).Render(sb)
                sb.Append(ht.Tail) |> ignore

    let prerender (view: HtmlElement -> HtmlElement) =
        let sb = StringBuilder()
        let mutable head = ""
        let fakeHole =
           { new HtmlElement with
                member this.Render(sb) =
                 head <- sb.ToString()
                 sb.Clear() |> ignore }
        let readyView = view fakeHole
        readyView.Render(sb)
        { Head = head; Tail = sb.ToString() }