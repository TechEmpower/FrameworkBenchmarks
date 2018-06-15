module HtmlViews

open Giraffe.GiraffeViewEngine
open Models

let layout (content: XmlNode list) =
    html [] [
        head [] [
            title []  [ rawText "Fortunes" ]
        ]
        body [] content
    ]

let fortunes (fortunes: Fortune seq) =
    [
        table [] [ 
            for f in fortunes ->
                tr [] [
                    td [] [ rawText <| string f.Id ]
                    td [] [ encodedText <| f.Message ]
                ] 
        ]
    ] |> layout
