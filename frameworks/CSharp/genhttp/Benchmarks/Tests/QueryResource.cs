using Benchmarks.Model;

using GenHTTP.Modules.Webservices;

using NpgsqlTypes;

namespace Benchmarks.Tests;

public sealed class QueryResource
{
    private static readonly Random Random = new();

    [ResourceMethod(":queries")]
    public Task<List<World>> GetWorldsFromPath(string queries) => GetWorlds(queries);

    [ResourceMethod]
    public Task<List<World>> GetWorlds(string queries)
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

        return GetRandomWorlds(count);
    }

    private static async Task<List<World>> GetRandomWorlds(int count)
    {
        var result = new List<World>(count);

        await using var connection = Database.Connection();

        await connection.OpenAsync();

        await using var command = connection.CreateCommand();

        command.CommandText = "SELECT id, randomnumber FROM world WHERE id = @Id";

        var parameter = command.Parameters.Add("@Id", NpgsqlDbType.Integer);

        for (var i = 0; i < count; i++)
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
        
        await connection.CloseAsync();

        return result;
    }

}
