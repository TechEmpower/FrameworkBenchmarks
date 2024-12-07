namespace App

open System
open System.Data
open Microsoft.Extensions.ObjectPool
open Npgsql


[<AutoOpen>]
module Db =
    let loadFortunes () =
        let result = ResizeArray()
        task {
            use db = new NpgsqlConnection(ConnectionString)
            use cmd = db.CreateCommand(CommandText = "SELECT id, message FROM fortune")
            do! db.OpenAsync()
            do! cmd.PrepareAsync()
            use! rdr = cmd.ExecuteReaderAsync(CommandBehavior.CloseConnection)
            while! rdr.ReadAsync() do
                result.Add { id = rdr.GetInt32(0); message = rdr.GetString(1) }
            return result
        }

    let private createReadCommand (connection: NpgsqlConnection) =
        let cmd = connection.CreateCommand(
            CommandText = "SELECT id, randomnumber FROM world WHERE id = @Id"
        )
        let id = NpgsqlParameter<int>(
            ParameterName = "@Id",
            DbType = DbType.Int32,
            TypedValue = Random.Shared.Next(1, 10001)
        )
        cmd.Parameters.Add(id) |> ignore
        struct(cmd, id)

    let private readSingleRow (cmd: NpgsqlCommand) =
        task {
            use! rdr = cmd.ExecuteReaderAsync(CommandBehavior.SingleRow)
            let! _ = rdr.ReadAsync()
            return { id = rdr.GetInt32(0); randomnumber = rdr.GetInt32(1) }
        }

    let loadSingleRow () =
        task {
            use db = new NpgsqlConnection(ConnectionString)
            let struct(cmd', _) = createReadCommand db
            use cmd = cmd'
            do! db.OpenAsync()
            do! cmd.PrepareAsync()
            return! readSingleRow cmd
        }

    let private readMultipleRows (count: int) (conn: NpgsqlConnection) =
        let result = Array.zeroCreate count
        task {
            let struct(cmd', idParam) = createReadCommand conn
            use cmd = cmd'
            for i in 0..result.Length-1 do
                let! row = readSingleRow cmd
                result[i] <- row
                idParam.TypedValue <- Random.Shared.Next(1, 10001)
            return result
        }

    let loadMultipleRows (count: int) =
        task {
            use db = new NpgsqlConnection(ConnectionString)
            do! db.OpenAsync()
            return! readMultipleRows count db
        }

    let private maxBatch = 500
    let private queries = Array.zeroCreate (maxBatch + 1)
    let private stringBuilderPool = DefaultObjectPoolProvider().CreateStringBuilderPool()
    let private batchUpdateString batchSize =
        match queries[batchSize] with
        | null ->
            let lastIndex = batchSize - 1
            let sb = stringBuilderPool.Get()
            sb.Append("UPDATE world SET randomNumber = temp.randomNumber FROM (VALUES ") |> ignore
            for i in 0..lastIndex-1 do
                sb.AppendFormat("(@Id_{0}, @Rn_{0}), ", i) |> ignore
            sb.AppendFormat("(@Id_{0}, @Rn_{0}) ORDER BY 1) AS temp(id, randomNumber) WHERE temp.id = world.id", lastIndex) |> ignore
            let result = sb.ToString()
            stringBuilderPool.Return(sb)
            queries[batchSize] <- result
            result
        | q ->
            q

    // fill cache
    let _ = [| 0..maxBatch |] |> Array.map batchUpdateString

    let private paramNames =
        seq { 0..maxBatch*2 }
        |> Seq.map (fun i -> struct($"@Rn_{i}", $"@Id_{i}"))
        |> Seq.toArray

    let private generateParameters (results: World[]) (command: NpgsqlCommand) =
        for i in 0..results.Length-1 do
            let randomNumber = Random.Shared.Next(1, 10001)
            let struct(rnParamName, idParamName) = paramNames[i]
            command.Parameters.Add(NpgsqlParameter<int>(
                ParameterName = rnParamName, DbType = DbType.Int32, TypedValue = randomNumber)) |> ignore
            command.Parameters.Add(NpgsqlParameter<int>(
                ParameterName = idParamName, DbType = DbType.Int32, TypedValue = results[i].id)) |> ignore
            results[i] <- { results[i] with randomnumber = randomNumber }

    let doMultipleUpdates (count: int) =
        task {
            use conn = new NpgsqlConnection(MultiplexedConnectionString)
            do! conn.OpenAsync()
            let! results = readMultipleRows count conn
            use cmd = conn.CreateCommand(CommandText = batchUpdateString count)
            do generateParameters results cmd
            let! _ = cmd.ExecuteNonQueryAsync()
            return results
        }
