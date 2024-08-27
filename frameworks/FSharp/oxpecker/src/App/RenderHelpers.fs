module RenderHelpers

    open System.Text
    open Oxpecker.ViewEngine

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
        (head, sb.ToString())

    let inline combine (head: string) (tail: string) (hole: HtmlElement) =
        { new HtmlElement with
            member this.Render(sb) =
                sb.Append(head) |> ignore
                hole.Render(sb)
                sb.Append(tail) |> ignore
        }