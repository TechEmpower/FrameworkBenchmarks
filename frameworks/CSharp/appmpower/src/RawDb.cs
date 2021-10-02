using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.Extensions.Caching.Memory;
using appMpower.Db;
using PlatformBenchmarks;

namespace appMpower
{
   public static class RawDb
   {
      private const int MaxBatch = 500;
      private static Random _random = new Random();
      private static string[] _queriesMultipleRows = new string[MaxBatch + 1];

      private static readonly object[] _cacheKeys = Enumerable.Range(0, 10001).Select((i) => new CacheKey(i)).ToArray();

      private static readonly appMpower.Memory.MemoryCache _cache = new appMpower.Memory.MemoryCache(
            new appMpower.Memory.MemoryCacheOptions()
            {
               ExpirationScanFrequency = TimeSpan.FromMinutes(60)
            });

      public static async Task<World> LoadSingleQueryRow()
      {
         var pooledConnection = await PooledConnections.GetConnection(DataProvider.ConnectionString);
         pooledConnection.Open();

         var (pooledCommand, _) = CreateReadCommand(pooledConnection);
         var world = await ReadSingleRow(pooledCommand);

         pooledCommand.Release();
         pooledConnection.Release();

         return world;
      }

      public static async Task<World[]> LoadMultipleQueriesRows(int count)
      {
         var worlds = new World[count];

         var pooledConnection = await PooledConnections.GetConnection(DataProvider.ConnectionString);
         pooledConnection.Open();

         var (pooledCommand, dbDataParameter) = CreateReadCommand(pooledConnection);

         for (int i = 0; i < count; i++)
         {
            worlds[i] = await ReadSingleRow(pooledCommand);
            dbDataParameter.Value = _random.Next(1, 10001);
         }

         pooledCommand.Release();
         pooledConnection.Release();

         return worlds;
      }

      public static async Task<List<Fortune>> LoadFortunesRows()
      {
         var fortunes = new List<Fortune>();

         var pooledConnection = await PooledConnections.GetConnection(DataProvider.ConnectionString);
         pooledConnection.Open();

         var pooledCommand = new PooledCommand("SELECT * FROM fortune", pooledConnection);
         var dataReader = await pooledCommand.ExecuteReaderAsync(CommandBehavior.SingleResult & CommandBehavior.SequentialAccess);

         while (dataReader.Read())
         {
            fortunes.Add(new Fortune
            (
                id: dataReader.GetInt32(0),
#if MYSQL
               //MariaDB ODBC connector does not correctly support Japanese characters in combination with default ADO.NET;
               //as a solution we custom read this string
                message: ReadColumn(dataReader, 1)
#else
                message: dataReader.GetString(1)
#endif
            ));
         }

         dataReader.Close();
         pooledCommand.Release();
         pooledConnection.Release();

         fortunes.Add(new Fortune(id: 0, message: "Additional fortune added at request time."));
         fortunes.Sort();

         return fortunes;
      }

      public static async Task<World[]> LoadMultipleUpdatesRows(int count)
      {
         var worlds = new World[count];

         var pooledConnection = await PooledConnections.GetConnection(DataProvider.ConnectionString);
         pooledConnection.Open();

         var (queryCommand, dbDataParameter) = CreateReadCommand(pooledConnection);

         for (int i = 0; i < count; i++)
         {
            worlds[i] = await ReadSingleRow(queryCommand);
            dbDataParameter.Value = _random.Next(1, 10001);
         }

         queryCommand.Release();

         var updateCommand = new PooledCommand(PlatformBenchmarks.BatchUpdateString.Query(count), pooledConnection);

         var ids = PlatformBenchmarks.BatchUpdateString.Ids;
         var randoms = PlatformBenchmarks.BatchUpdateString.Randoms;

#if !MYSQL
         var jds = PlatformBenchmarks.BatchUpdateString.Jds;
#endif      

         for (int i = 0; i < count; i++)
         {
            var randomNumber = _random.Next(1, 10001);

            updateCommand.CreateParameter(ids[i], DbType.Int32, worlds[i].Id);
            updateCommand.CreateParameter(randoms[i], DbType.Int32, randomNumber);

            worlds[i].RandomNumber = randomNumber;
         }

#if !MYSQL
         for (int i = 0; i < count; i++)
         {
            updateCommand.CreateParameter(jds[i], DbType.Int32, worlds[i].Id);
         }
#endif

         await updateCommand.ExecuteNonQueryAsync();

         updateCommand.Release();
         pooledConnection.Release();

         return worlds;
      }

      private static (PooledCommand pooledCommand, IDbDataParameter dbDataParameter) CreateReadCommand(PooledConnection pooledConnection)
      {
         var pooledCommand = new PooledCommand("SELECT * FROM world WHERE id=?", pooledConnection);
         var dbDataParameter = pooledCommand.CreateParameter("@Id", DbType.Int32, _random.Next(1, 10001));

         return (pooledCommand, dbDataParameter);
      }

      private static async Task<World> ReadSingleRow(PooledCommand pooledCommand)
      {
         var dataReader = await pooledCommand.ExecuteReaderAsync(CommandBehavior.SingleRow & CommandBehavior.SequentialAccess);

         dataReader.Read();

         var world = new World
         {
            Id = dataReader.GetInt32(0),
            RandomNumber = dataReader.GetInt32(1)
         };

         dataReader.Close();

         return world;
      }

      public static async Task<World[]> ReadMultipleRows(int count)
      {
         int j = 0;
         var ids = PlatformBenchmarks.BatchUpdateString.Ids;
         var worlds = new World[count];
         string queryString;

         if (_queriesMultipleRows[count] != null)
         {
            queryString = _queriesMultipleRows[count];
         }
         else
         {
            var stringBuilder = PlatformBenchmarks.StringBuilderCache.Acquire();

            for (int i = 0; i < count; i++)
            {
               stringBuilder.Append("SELECT * FROM world WHERE id=?;");
            }

            queryString = _queriesMultipleRows[count] = PlatformBenchmarks.StringBuilderCache.GetStringAndRelease(stringBuilder);
         }

         var pooledConnection = await PooledConnections.GetConnection(DataProvider.ConnectionString);
         pooledConnection.Open();

         var pooledCommand = new PooledCommand(queryString, pooledConnection);

         for (int i = 0; i < count; i++)
         {
            pooledCommand.CreateParameter(ids[i], DbType.Int32, _random.Next(1, 10001));
         }

         var dataReader = await pooledCommand.ExecuteReaderAsync(CommandBehavior.Default & CommandBehavior.SequentialAccess);

         do
         {
            dataReader.Read();

            worlds[j] = new World
            {
               Id = dataReader.GetInt32(0),
               RandomNumber = dataReader.GetInt32(1)
            };

            j++;
         } while (await dataReader.NextResultAsync());

         dataReader.Close();
         pooledCommand.Release();
         pooledConnection.Release();

         return worlds;
      }

      public static string ReadColumn(DbDataReader dbDataReader, int column)
      {
         long size = dbDataReader.GetBytes(column, 0, null, 0, 0);  //get the length of data
         byte[] values = new byte[size];

         int bufferSize = 64;
         long bytesRead = 0;
         int currentPosition = 0;

         while (bytesRead < size)
         {
            bytesRead += dbDataReader.GetBytes(column, currentPosition, values, currentPosition, bufferSize);
            currentPosition += bufferSize;
         }

         return System.Text.Encoding.Default.GetString(values);
      }

      public static async Task PopulateCache()
      {
         var pooledConnection = await PooledConnections.GetConnection(DataProvider.ConnectionString);
         pooledConnection.Open();

         var (pooledCommand, dbDataParameter) = CreateReadCommand(pooledConnection);

         using (pooledCommand)
         {
            var cacheKeys = _cacheKeys;
            var cache = _cache;

            for (var i = 1; i < 10001; i++)
            {
               dbDataParameter.Value = i;
               cache.Set<CachedWorld>(cacheKeys[i], await ReadSingleRow(pooledCommand));
            }
         }

         pooledCommand.Release();
         pooledConnection.Release();
      }

      public static Task<CachedWorld[]> LoadCachedQueries(int count)
      {
         var result = new CachedWorld[count];
         var cacheKeys = _cacheKeys;
         var cache = _cache;
         var random = _random;

         for (var i = 0; i < result.Length; i++)
         {
            var id = random.Next(1, 10001);
            var key = cacheKeys[id];

            if (cache.TryGetValue(key, out object cached))
            {
               result[i] = (CachedWorld)cached;
            }
            else
            {
               //return LoadUncachedQueries(id, i, count, this, result);
               return LoadUncachedQueries(id, i, count, result);
            }
         }

         return Task.FromResult(result);
      }

      //static async Task<CachedWorld[]> LoadUncachedQueries(int id, int i, int count, RawDb rawdb, CachedWorld[] result)
      static async Task<CachedWorld[]> LoadUncachedQueries(int id, int i, int count, CachedWorld[] result)
      {
         var pooledConnection = await PooledConnections.GetConnection(DataProvider.ConnectionString);
         pooledConnection.Open();

         var (pooledCommand, dbDataParameter) = CreateReadCommand(pooledConnection);

         using (pooledCommand)
         {
            Func<ICacheEntry, Task<CachedWorld>> create = async (entry) =>
            {
               return await ReadSingleRow(pooledCommand);
            };

            var cacheKeys = _cacheKeys;
            var key = cacheKeys[id];

            dbDataParameter.Value = id;

            for (; i < result.Length; i++)
            {
               result[i] = await _cache.GetOrCreateAsync<CachedWorld>(key, create);

               id = _random.Next(1, 10001);
               dbDataParameter.Value = id;
               key = cacheKeys[id];
            }

            pooledCommand.Release();
            pooledConnection.Release();
         }

         return result;
      }
   }
}