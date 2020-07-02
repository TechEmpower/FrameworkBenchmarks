module App.UI

open Falco.ViewEngine
    
let layout pageTitle content = 
    html [] [
        head [] [                
                title [] [ raw pageTitle ]                                                                
            ]
        body [] content
    ] 