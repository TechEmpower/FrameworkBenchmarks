module App.UI

open Falco.ViewEngine
    
let layout pageTitle content = 
    html [] [
        head [] [
                meta  [ _charset "UTF-8" ]                        
                title [] [ raw pageTitle ]                                                                
            ]
        body [] content
    ] 