module App.Fortune    

open System.Data
open System.Threading.Tasks     
open Donald
open Falco
 
type FortuneModel = 
   {
       id      : int
       message : string
   }

module FortuneModel =
   let fromDataReader (rd : IDataReader) =
       {
           id = rd.GetInt32("id")
           message = rd.GetString("message")
       }

module Service = 
    module ListQuery =              
        type LoadFortunes = unit -> Task<FortuneModel list>

        let extraFortune = 
            {
                id = 0
                message = "Additional fortune added at request time."
            }

        let handle
            (loadFortunes : LoadFortunes) =
            fun () -> 
                task {
                    let! fortunes = loadFortunes ()
                    
                    return 
                        extraFortune 
                        :: fortunes
                        |> List.sortBy (fun f -> f.message)
                }


module Db =    
    let selectAsync (connection : IDbConnection) : Task<FortuneModel list> =        
        queryAsync 
            "SELECT id, message FROM fortune"
            []
            FortuneModel.fromDataReader
            connection

module View =
    open Falco.Markup
    
    let index (fortunes : FortuneModel list) =            
        UI.layout "Fortunes" [
                Elem.table [] [
                        yield Elem.tr [] [
                                Elem.th [] [ Text.raw "id" ]
                                Elem.th [] [ Text.raw "message" ]
                            ]
                        for fortune in fortunes ->
                            Elem.tr [] [
                                    Elem.td [] [ Text.raw (string fortune.id) ]
                                    Elem.td [] [ Text.enc fortune.message]
                                ]
                    ]
            ]

let handleIndex : HttpHandler =        
    fun ctx ->
        task {
            let connFactory = ctx.GetService<DbConnectionFactory>()
            use conn = createConn connFactory
            let selectFortunes = fun () -> Db.selectAsync conn
            let! fortunes = () |> Service.ListQuery.handle selectFortunes

            return!
                ctx
                |> (fortunes 
                    |> View.index 
                    |> Response.ofHtml)                    
        } :> Task
