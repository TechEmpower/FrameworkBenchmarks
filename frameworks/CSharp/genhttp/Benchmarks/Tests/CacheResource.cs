using System;
using System.Collections.Generic;
using System.Linq;

using Benchmarks.Model;

using GenHTTP.Modules.Webservices;

using Microsoft.Extensions.Caching.Memory;

namespace Benchmarks.Tests
{

    public class CacheResource
    {
        private static readonly Random _Random = new Random();

        private readonly MemoryCache _Cache = new MemoryCache(new MemoryCacheOptions() { ExpirationScanFrequency = TimeSpan.FromMinutes(60) });

        private static readonly object[] _CacheKeys = Enumerable.Range(0, 10001).Select((i) => new CacheKey(i)).ToArray();

        public sealed class CacheKey : IEquatable<CacheKey>
        {
            private readonly int _value;

            public CacheKey(int value) => _value = value;

            public bool Equals(CacheKey key) => key._value == _value;

            public override bool Equals(object obj) => ReferenceEquals(obj, this);

            public override int GetHashCode() => _value;

            public override string ToString() => _value.ToString();

        }

        [ResourceMethod(":queries")]
        public List<World> GetWorldsFromPath(string queries) => GetWorlds(queries);

        [ResourceMethod]
        public List<World> GetWorlds(string queries)
        {
            var count = 1;

            int.TryParse(queries, out count);

            if (count < 1) count = 1;
            else if (count > 500) count = 500;

            var result = new List<World>(count);

            using var context = DatabaseContext.Create();

            for (var i = 0; i < count; i++)
            {
                var id = _Random.Next(1, 10001);

                var key = _CacheKeys[id];

                var data = _Cache.Get<World>(key);

                if (data != null)
                {
                    result.Add(data);
                }
                else
                {
                    var resolved = context.World.First(w => w.Id == id);

                    _Cache.Set(key, resolved);

                    result.Add(resolved);
                }
            }

            return result;
        }

    }

}
