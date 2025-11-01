module NewtonsoftJson

open System.IO
open System.Text
open System.Threading.Tasks
open Microsoft.IO
open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open Giraffe

type Serializer
    (settings: JsonSerializerSettings, rmsManager: RecyclableMemoryStreamManager)
    =
    let serializer = JsonSerializer.Create settings
    let utf8EncodingWithoutBom = UTF8Encoding (false)

    static member DefaultSettings =
        JsonSerializerSettings (
            ContractResolver = CamelCasePropertyNamesContractResolver ()
        )

    interface Json.ISerializer with
        member __.SerializeToString (x: 'T) =
            JsonConvert.SerializeObject (x, settings)

        member __.SerializeToBytes (x: 'T) =
            JsonConvert.SerializeObject (x, settings) |> Encoding.UTF8.GetBytes

        member __.SerializeToStreamAsync (x: 'T) (stream: Stream) =
            task {
                use memoryStream =
                    rmsManager.GetStream ("giraffe-json-serialize-to-stream")

                use streamWriter = new StreamWriter (memoryStream, utf8EncodingWithoutBom)

                use jsonTextWriter = new JsonTextWriter (streamWriter)
                serializer.Serialize (jsonTextWriter, x)
                jsonTextWriter.Flush ()
                memoryStream.Seek (0L, SeekOrigin.Begin) |> ignore
                do! memoryStream.CopyToAsync (stream, 65536)
            }
            :> Task

        member __.Deserialize<'T> (json: string) =
            JsonConvert.DeserializeObject<'T> (json, settings)

        member __.Deserialize<'T> (bytes: byte array) =
            let json = Encoding.UTF8.GetString bytes
            JsonConvert.DeserializeObject<'T> (json, settings)

        member __.DeserializeAsync<'T> (stream: Stream) =
            task {
                use memoryStream = rmsManager.GetStream ("giraffe-json-deserialize")

                do! stream.CopyToAsync (memoryStream)
                memoryStream.Seek (0L, SeekOrigin.Begin) |> ignore
                use streamReader = new StreamReader (memoryStream)
                use jsonTextReader = new JsonTextReader (streamReader)
                return serializer.Deserialize<'T> (jsonTextReader)
            }
