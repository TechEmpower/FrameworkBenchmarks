using System;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;
using Benchmarks.Model;

using GenHTTP.Modules.Webservices;

using Npgsql;
using NpgsqlTypes;

namespace Benchmarks.Tests;

public sealed class UpdateResource
{

    [ResourceMethod(":queries")]
    public ValueTask<World[]> UpdateWorldsFromPath(string queries) => UpdateWorlds(queries);

    [ResourceMethod]
    public ValueTask<World[]> UpdateWorlds(string queries)
    {
        if (!int.TryParse(queries, out var count))
        {
            count = 1;
        }

        if (count < 1)
        {
            count = 1;
        }
        else if (count > 500)
        {
            count = 500;
        }
        
        return FetchAndUpdateWorlds(count);
    }

    private async ValueTask<World[]> FetchAndUpdateWorlds(int count)
    {
        var results = new World[count];

        var ids = new int[count];
        var numbers = new int[count];

        for (var i = 0; i < count; i++)
        {
            ids[i] = Random.Shared.Next(1, 10001);
        }
        
        Array.Sort(ids);
        
        for (var i = 1; i < count; i++)
            if (ids[i] == ids[i - 1])
                ids[i] = (ids[i] % 10000) + 1;

        using var connection = await Database.DataSource.OpenConnectionAsync();

        // Each row must be selected randomly using one query in the same fashion as the single database query test
        // Use of IN clauses or similar means to consolidate multiple queries into one operation is not permitted.
        // Similarly, use of a batch or multiple SELECTs within a single statement are not permitted
        var (queryCmd, queryParameter) = CreateReadCommand(connection);
        using (queryCmd)
        {
            for (var i = 0; i < results.Length; i++)
            {
                queryParameter.TypedValue = ids[i];
                results[i] = await ReadSingleRow(queryCmd);
            }
        }

        for (var i = 0; i < count; i++)
        {
            var randomNumber = Random.Shared.Next(1, 10001);
            if (results[i].RandomNumber == randomNumber)
            {
                randomNumber = (randomNumber % 10000) + 1;
            }

            results[i].RandomNumber = randomNumber;
            numbers[i] = randomNumber;
        }

        var update = "UPDATE world w SET randomnumber = u.new_val FROM (SELECT unnest($1) as id, unnest($2) as new_val) u WHERE w.id = u.id";

        using var updateCmd = new NpgsqlCommand(update, connection);
        updateCmd.Parameters.Add(new NpgsqlParameter<int[]> { TypedValue = ids, NpgsqlDbType = NpgsqlDbType.Array | NpgsqlDbType.Integer });
        updateCmd.Parameters.Add(new NpgsqlParameter<int[]> { TypedValue = numbers, NpgsqlDbType = NpgsqlDbType.Array | NpgsqlDbType.Integer });

        await updateCmd.ExecuteNonQueryAsync();

        return results;
    }
    
    private (NpgsqlCommand readCmd, NpgsqlParameter<int> idParameter) CreateReadCommand(NpgsqlConnection connection)
    {
        var cmd = new NpgsqlCommand("SELECT id, randomnumber FROM world WHERE id = $1", connection);
        var parameter = new NpgsqlParameter<int> { TypedValue = Random.Shared.Next(1, 10001) };

        cmd.Parameters.Add(parameter);

        return (cmd, parameter);
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static async Task<World> ReadSingleRow(NpgsqlCommand cmd)
    {
        using var rdr = await cmd.ExecuteReaderAsync(System.Data.CommandBehavior.SingleRow);
        await rdr.ReadAsync();

        return new World
        {
            Id = rdr.GetInt32(0),
            RandomNumber = rdr.GetInt32(1)
        };
    }

}
