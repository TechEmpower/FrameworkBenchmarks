open Suave
open Suave.Filters
open Suave.Operators
open Suave.Json

open System.Net
open System.Runtime.Serialization

[<DataContract>]
type JsonMessage = {  [<field:DataMember(Name = "message")>] message: string }

let app =
  choose [
    path "/plaintext" >=> Writers.setMimeType "text/plain; charset=utf-8" >=> Successful.OK "Hello, World!"
    path "/json" >=> Writers.setMimeType "application/json; charset=utf-8" >=> Successful.ok (toJson { message = "Hello, World!"})
  ]

let cfg = { defaultConfig with bindings = [ HttpBinding.create HTTP IPAddress.Any 8080us ] }

startWebServer cfg app
