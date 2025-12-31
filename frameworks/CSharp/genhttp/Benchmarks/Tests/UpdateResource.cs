using Benchmarks.Model;

using GenHTTP.Modules.Webservices;

using Npgsql;
using NpgsqlTypes;

namespace Benchmarks.Tests;

public sealed class UpdateResource
{
    private static readonly Random Random = new();

    [ResourceMethod(":queries")]
    public ValueTask<List<World>> UpdateWorldsFromPath(string queries) => UpdateWorlds(queries);

    [ResourceMethod]
    public async ValueTask<List<World>> UpdateWorlds(string queries)
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
        
        await using var connection = Database.Connection();

        await connection.OpenAsync();

        var worlds = await GetRandomWorlds(connection, count);

        Shuffle(worlds);
        
        await Persist(connection, worlds);
        
        await connection.CloseAsync();

        return worlds;
    }

    private static void Shuffle(List<World> worlds)
    {
        for (var i = 0; i < worlds.Count; i++)
        {
            var world = worlds[i];
            
            var old = world.RandomNumber;

            var current = old;

            for (var j = 0; j < 5; j++)
            {
                current = Random.Next(1, 10001);

                if (current != old)
                {
                    break;
                }
            }

            world.RandomNumber = current;            
        }
    }
    
    private static async Task<List<World>> GetRandomWorlds(NpgsqlConnection connection, int count)
    {
        var result = new List<World>(count);

        await using var command = connection.CreateCommand();

        command.CommandText = "SELECT id, randomnumber FROM world WHERE id = @Id";

        var parameter = command.Parameters.Add("@Id", NpgsqlDbType.Integer);

        for (int i = 0; i < count; i++)
        {
            parameter.Value = Random.Next(1, 10001);

            await using var reader = await command.ExecuteReaderAsync();

            if (await reader.ReadAsync())
            {
                result.Add(new()
                {
                    Id = reader.GetInt32(0),
                    RandomNumber = reader.GetInt32(1)
                });
            }
        }
        
        return result;
    }

    private static async Task Persist(NpgsqlConnection connection, List<World> worlds)
    {
        await using var command = connection.CreateCommand();

        command.CommandText = "UPDATE world SET randomnumber = u.randomnumber FROM UNNEST(@Ids, @RandomNumbers) AS u(id, randomnumber) WHERE world.id = u.id";

        var ids = new int[worlds.Count];
        var randomNumbers = new int[worlds.Count];

        for (int i = 0; i < worlds.Count; i++)
        {
            ids[i] = worlds[i].Id;
            randomNumbers[i] = worlds[i].RandomNumber;
        }

        command.Parameters.AddWithValue("@Ids", ids);
        command.Parameters.AddWithValue("@RandomNumbers", randomNumbers);

        await command.ExecuteNonQueryAsync();
    }
    
}