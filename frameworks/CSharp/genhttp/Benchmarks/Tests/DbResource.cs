using Benchmarks.Model;

using GenHTTP.Modules.Webservices;

namespace Benchmarks.Tests;

public sealed class DbResource
{
    private static readonly Random Random = new();

    [ResourceMethod]
    public Task<World> GetRandomWorld() => GetWorldById(Random.Next(1, 10001));

    private static async Task<World> GetWorldById(int id)
    {
        await using var connection = Database.Connection();

        await connection.OpenAsync();

        try
        {
            await using var command = connection.CreateCommand();

            command.CommandText = "SELECT id, randomnumber FROM world WHERE id = @Id";

            command.Parameters.AddWithValue("@Id", id);

            await using var reader = await command.ExecuteReaderAsync();

            if (await reader.ReadAsync())
            {
                return new()
                {
                    Id = reader.GetInt32(0),
                    RandomNumber = reader.GetInt32(1)
                };
            }
        }
        finally
        {
            await connection.CloseAsync();
        }

        return null;
    }

}
