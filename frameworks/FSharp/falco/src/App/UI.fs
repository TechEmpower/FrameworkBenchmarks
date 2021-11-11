module App.UI

open Falco.Markup
    
let layout pageTitle content = 
    Elem.html [] [
        Elem.head [] [                
                Elem.title [] [ Text.raw pageTitle ]                                                                
            ]
        Elem.body [] content
    ] 