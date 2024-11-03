using Benchmarks.Model;
using GenHTTP.Modules.Webservices;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Caching.Memory;

namespace Benchmarks.Tests;

public sealed class CacheResource
{
    private static readonly Random Random = new();

    private static readonly MemoryCache Cache = new(new MemoryCacheOptions
    {
        ExpirationScanFrequency = TimeSpan.FromMinutes(60)
    });

    private static readonly object[] CacheKeys = Enumerable.Range(0, 10001).Select(i => new CacheKey(i)).ToArray();

    [ResourceMethod(":queries")]
    public ValueTask<List<World>> GetWorldsFromPath(string queries) => GetWorlds(queries);

    [ResourceMethod]
    public async ValueTask<List<World>> GetWorlds(string queries)
    {
        var count = 1;

        int.TryParse(queries, out count);

        if (count < 1)
        {
            count = 1;
        }
        else if (count > 500)
        {
            count = 500;
        }

        var result = new List<World>(count);

        using var context = DatabaseContext.CreateNoTracking();

        for (var i = 0; i < count; i++)
        {
            var id = Random.Next(1, 10001);

            var key = CacheKeys[id];

            var data = Cache.Get<World>(key);

            if (data != null)
            {
                result.Add(data);
            }
            else
            {
                var resolved = await context.World.FirstOrDefaultAsync(w => w.Id == id).ConfigureAwait(false);

                Cache.Set(key, resolved);

                result.Add(resolved);
            }
        }

        return result;
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
