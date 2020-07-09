module App.Fortune    
    
module Model = 
    open System.Threading.Tasks 
    open FSharp.Control.Tasks

    type FortuneModel = 
        {
            id      : int
            message : string
        }

    module FortuneModel =
        open System.Data
        open Donald 

        let extra = 
            {
                id = 0
                message = "Additional fortune added at request time."
            }

        let fromDataReader (rd : IDataReader) =
            {
                id = rd.GetInt32("id")
                message = rd.GetString("message")
            }

    module Index =        
        type Query =
            unit -> Task<FortuneModel list>

        type LoadFortunes =
            unit -> Task<FortuneModel list>

        let query 
            (loadFortunes : LoadFortunes) : Query =
            fun () -> 
                task {
                    let! fortunes = loadFortunes ()
                    let fortunesWithExtra = FortuneModel.extra :: fortunes
                    let fortunesSorted = 
                        fortunesWithExtra 
                        |> List.sortBy (fun f -> f.message)

                    return fortunesSorted 
                }

module Db =
    open System.Data
    open Donald
    open Model

    let selectAsync (connection : IDbConnection) =
        queryAsync 
            "SELECT id, message FROM fortune"
            []
            FortuneModel.fromDataReader
            connection

module View =
    open Falco.ViewEngine
    open Model 

    let index (fortunes : FortuneModel list) =            
        UI.layout "Fortunes" [
                table [] [
                        yield tr [] [
                                th [] [ raw "id" ]
                                th [] [ raw "message" ]
                            ]
                        for fortune in fortunes ->
                            tr [] [
                                    td [] [ raw (string fortune.id) ]
                                    td [] [ enc fortune.message]
                                ]
                    ]
            ]

module Controller =
    open Donald
    open Falco
    open FSharp.Control.Tasks
    open Model

    let handleIndex : HttpHandler =        
        fun next ctx ->
            task {
                let connFactory = ctx.GetService<DbConnectionFactory>()
                use conn = createConn connFactory
                let selectFortunes = fun () -> Db.selectAsync conn
                let! fortunes = () |> Index.query selectFortunes

                let handlerResult = 
                    fortunes
                    |> View.index
                    |> htmlOut

                return! handlerResult next ctx
            }