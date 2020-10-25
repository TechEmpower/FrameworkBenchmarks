module HtmlViews

open Giraffe.ViewEngine
open Models

let private fortunesHead = 
    head [] [
        title []  [ rawText "Fortunes" ]
    ]

let private layout (content: XmlNode list) =
    html [] [
        fortunesHead
        body [] content
    ]

let private fortunesTableHeader = 
    tr [] [
        th [] [ rawText "id" ]
        th [] [ rawText "message" ]
    ]

let fortunes (fortunes: Fortune seq) =
    [
        table [] [ 
            yield fortunesTableHeader
            for f in fortunes ->
                tr [] [
                    td [] [ rawText <| string f.id ]
                    td [] [ encodedText <| f.message ]
                ] 
        ]
    ] |> layout
