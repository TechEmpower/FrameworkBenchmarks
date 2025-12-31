using Benchmarks.Model;

using GenHTTP.Modules.Webservices;

using Microsoft.Extensions.Caching.Memory;

using Npgsql;

namespace Benchmarks.Tests;

public sealed class CacheResource
{
    private static readonly MemoryCache Cache = new(new MemoryCacheOptions
    {
        ExpirationScanFrequency = TimeSpan.FromMinutes(60)
    });

    private static readonly CacheKey[] CacheKeys = Enumerable.Range(0, 10001).Select(i => new CacheKey(i)).ToArray();

    [ResourceMethod(":queries")]
    public ValueTask<List<World>> GetWorldsFromPath(string queries) => GetWorlds(queries);

    [ResourceMethod]
    public async ValueTask<List<World>> GetWorlds(string queries)
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

        NpgsqlConnection connection = null;

        var result = new List<World>(count);

        for (var i = 0; i < count; i++)
        {
            var id = Random.Shared.Next(1, 10001);

            var key = CacheKeys[id];

            var data = Cache.Get<World>(key);

            if (data != null)
            {
                result.Add(data);
            }
            else
            {
                if (connection == null)
                {
                    connection = Database.Connection();

                    await connection.OpenAsync();
                }

                var resolved = await GetWorldById(connection, id);

                Cache.Set(key, resolved);

                result.Add(resolved);
            }
        }

        if (connection != null)
        {
            await connection.CloseAsync();
        }

        return result;
    }

    private static async Task<World> GetWorldById(NpgsqlConnection connection, int id)
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

        return null;
    }

    public sealed class CacheKey : IEquatable<CacheKey>
    {
        private readonly int _value;

        public CacheKey(int value)
        {
            _value = value;
        }

        public bool Equals(CacheKey key) => key._value == _value;

        public override bool Equals(object obj) => ReferenceEquals(obj, this);

        public override int GetHashCode() => _value;

        public override string ToString() => _value.ToString();
    }

}
