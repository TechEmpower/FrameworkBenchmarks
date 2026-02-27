open Suave
open Suave.Operators
open Suave.Router
open Suave.Json

open System.Net
open System.Runtime.Serialization

[<DataContract>]
type JsonMessage = {  [<field:DataMember(Name = "message")>] message: string }

let app : WebPart = router {
    get "/plaintext" (Writers.setMimeType "text/plain; charset=utf-8" >=> Successful.OK "Hello, World!")
    get "/json" (Writers.setMimeType "application/json; charset=utf-8" >=> Successful.ok (toJson { message = "Hello, World!"}))
  }

let cfg = { defaultConfig with bindings = [ HttpBinding.create HTTP IPAddress.Any 8080us ] }

startWebServer cfg app
