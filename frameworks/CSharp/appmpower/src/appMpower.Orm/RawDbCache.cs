using System.Data;
using Microsoft.Extensions.Caching.Memory;
using appMpower.Orm.Data;
using appMpower.Orm.Objects;
using PlatformBenchmarks;

namespace appMpower.Orm
{
   public sealed class RawDbCache
   {
      private readonly ConcurrentRandom _random; 
      private readonly MemoryCache _cache
        = new(new MemoryCacheOptions { ExpirationScanFrequency = TimeSpan.FromMinutes(60) });
      private static readonly object[] _cacheKeys = Enumerable.Range(0, 10001).Select((i) => new CacheKey(i)).ToArray();

      public RawDbCache()
      {
        _random = new();
        this.PopulateCache();
      }

      public RawDbCache(ConcurrentRandom random)
      {
        _random = random;
      }

      public CachedWorld[] LoadCachedQueries(int count)
      {
         var result = new CachedWorld[count];
         var cacheKeys = _cacheKeys;
         var cache = _cache;
         var random = _random;

         for (var i = 0; i < result.Length; i++)
         {
            var id = random.Next(1, 10001);
            var key = cacheKeys[id];
            if (cache.TryGetValue(key, out var cached))
            {
               result[i] = (CachedWorld)cached;
            }
            else
            {
               return LoadUncachedQueries(id, i, count, this, result);
            }
         }

         return result;

         static CachedWorld[] LoadUncachedQueries(int id, int i, int count, RawDbCache rawdbCache, CachedWorld[] result)
         {
            using var pooledConnection = DbConnections.GetConnection(DbProviderFactory.ConnectionString);
            pooledConnection.Open();

            var (dbCommand, dbDataParameter) = RawDb.CreateReadCommand(pooledConnection);

            using var command = dbCommand;
            CachedWorld create(ICacheEntry _) => RawDb.ReadSingleRow(command);

            var cacheKeys = _cacheKeys;
            var key = cacheKeys[id];

            dbDataParameter.Value = id;

            for (; i < result.Length; i++)
            {
               result[i] = rawdbCache._cache.GetOrCreate<CachedWorld>(key, create);

               id = rawdbCache._random.Next(1, 10001);
               dbDataParameter.Value = id;
               key = cacheKeys[id];
            }

            return result;
         }
      }

      public void PopulateCache()
      {
         using var pooledConnection = DbConnections.GetConnection(DbProviderFactory.ConnectionString);
         pooledConnection.Open();

         var (dbCommand, dbDataParameter) = RawDb.CreateReadCommand(pooledConnection);
         using var command = dbCommand;

         var cacheKeys = _cacheKeys;
         var cache = _cache;

         for (var i = 1; i < 10001; i++)
         {
            dbDataParameter.Value = i;
            cache.Set<CachedWorld>(cacheKeys[i], RawDb.ReadSingleRow(command));
         }

         Console.WriteLine("Caching Populated");
      }
   }
}