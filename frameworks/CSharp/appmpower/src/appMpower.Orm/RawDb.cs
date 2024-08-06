using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using Microsoft.Extensions.Caching.Memory;
using appMpower.Orm; 
using appMpower.Orm.Data; 
using appMpower.Orm.Objects;
using PlatformBenchmarks;

namespace appMpower.Orm
{
   public static class RawDb
   {
      private const int MaxBatch = 500;

      private static ConcurrentRandom _concurrentRandom = new ConcurrentRandom();
      private static Random _random = new Random();

      private static string[] _queriesMultipleRows = new string[MaxBatch + 1];

      /*
      TODO
      private static readonly object[] _cacheKeys = Enumerable.Range(0, 10001).Select((i) => new CacheKey(i)).ToArray();

      private static readonly appMpower.Memory.MemoryCache _cache = new appMpower.Memory.MemoryCache(
            new appMpower.Memory.MemoryCacheOptions()
            {
               ExpirationScanFrequency = TimeSpan.FromMinutes(60)
            });
      */

      public static World LoadSingleQueryRow()
      {
         using var pooledConnection = DbConnections.GetConnection(DbProviderFactory.ConnectionString);
         pooledConnection.Open();

         var (dbCommand, _) = CreateReadCommand(pooledConnection);

         using (dbCommand)
         {
            World world = ReadSingleRow(dbCommand);

            return world;
         }
      }

      public static World[] LoadMultipleQueriesRows(int count)
      {
         var worlds = new World[count];

         using var pooledConnection = DbConnections.GetConnection(DbProviderFactory.ConnectionString);
         pooledConnection.Open();

         var (dbCommand, dbDataParameter) = CreateReadCommand(pooledConnection);

         using (dbCommand)
         {
            for (int i = 0; i < count; i++)
            {
               worlds[i] = ReadSingleRow(dbCommand);
               dbDataParameter.Value = _concurrentRandom.Next(1, 10001);
            }
         }

         return worlds;
      }

      public static List<Fortune> LoadFortunesRows()
      {
         var fortunes = new List<Fortune>();

         using var pooledConnection = DbConnections.GetConnection(DbProviderFactory.ConnectionString);
         pooledConnection.Open();

         var dbCommand = new DbCommand("SELECT * FROM fortune", pooledConnection);

         using (dbCommand)
         {
            IDataReader dataReader = dbCommand.ExecuteReader(CommandBehavior.SingleResult & CommandBehavior.SequentialAccess);

            while (dataReader.Read())
            {
               fortunes.Add(new Fortune
               (
                  id: dataReader.GetInt32(0),
                  //MariaDB ODBC connector does not correctly support Japanese characters in combination with default ADO.NET;
                  //as a solution we custom read this string
                  message: (Constants.Dbms == Dbms.MySQL ? ReadColumn(dataReader, 1) : dataReader.GetString(1))
               ));
            }

            dataReader.Close();
         }

         fortunes.Add(new Fortune(id: 0, message: "Additional fortune added at request time."));
         fortunes.Sort();

         return fortunes;
      }

      public static World[] LoadMultipleUpdatesRows(int count)
      {
         var worlds = new World[count];

         using var pooledConnection = DbConnections.GetConnection(DbProviderFactory.ConnectionString);
         pooledConnection.Open();

         var (queryCommand, dbDataParameter) = CreateReadCommand(pooledConnection);

         using (queryCommand)
         {
            for (int i = 0; i < count; i++)
            {
               worlds[i] = ReadSingleRow(queryCommand);
               dbDataParameter.Value = _random.Next(1, 10001);
            }
         }

         using var updateCommand = new DbCommand(PlatformBenchmarks.BatchUpdateString.Query(count), pooledConnection);

         var ids = PlatformBenchmarks.BatchUpdateString.Ids;
         var randoms = PlatformBenchmarks.BatchUpdateString.Randoms;

         for (int i = 0; i < count; i++)
         {
            var randomNumber = _random.Next(1, 10001);

            updateCommand.CreateParameter(ids[i], DbType.Int32, worlds[i].Id);
            updateCommand.CreateParameter(randoms[i], DbType.Int32, randomNumber);

            worlds[i].RandomNumber = randomNumber;
         }

         if (Constants.Dbms != Dbms.MySQL)
         {
            var jds = PlatformBenchmarks.BatchUpdateString.Jds;

            for (int i = 0; i < count; i++)
            {
               updateCommand.CreateParameter(jds[i], DbType.Int32, worlds[i].Id);
            }
         }

         updateCommand.ExecuteNonQuery();

         return worlds;
      }

      private static (DbCommand dbCommand, IDbDataParameter dbDataParameter) CreateReadCommand(DbConnection pooledConnection)
      {
         DbCommand dbCommand = new DbCommand("SELECT * FROM world WHERE id=?", pooledConnection);

         return (dbCommand, dbCommand.CreateParameter("Id", DbType.Int32, _random.Next(1, 10001)));
      }

      private static World ReadSingleRow(DbCommand dbCommand)
      {
         var dataReader = dbCommand.ExecuteReader(CommandBehavior.SingleRow & CommandBehavior.SequentialAccess);

         dataReader.Read();

         var world = new World
         {
            Id = dataReader.GetInt32(0),
            RandomNumber = dataReader.GetInt32(1)
         };

         dataReader.Close();

         return world;
      }

      public static World[] ReadMultipleRows(int count)
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

         using var pooledConnection = DbConnections.GetConnection(DbProviderFactory.ConnectionString);
         pooledConnection.Open();

         using var dbCommand = new DbCommand(queryString, pooledConnection);

         for (int i = 0; i < count; i++)
         {
            dbCommand.CreateParameter(ids[i], DbType.Int32, _random.Next(1, 10001));
         }

         var dataReader = dbCommand.ExecuteReader(CommandBehavior.Default & CommandBehavior.SequentialAccess);

         do
         {
            dataReader.Read();

            worlds[j] = new World
            {
               Id = dataReader.GetInt32(0),
               RandomNumber = dataReader.GetInt32(1)
            };

            j++;
         } while (dataReader.NextResult());

         dataReader.Close();

         return worlds;
      }

      public static string ReadColumn(IDataReader dataReader, int column)
      {
         long size = dataReader.GetBytes(column, 0, null, 0, 0);  //get the length of data
         byte[] values = new byte[size];

         int bufferSize = 64;
         long bytesRead = 0;
         int currentPosition = 0;

         while (bytesRead < size)
         {
            bytesRead += dataReader.GetBytes(column, currentPosition, values, currentPosition, bufferSize);
            currentPosition += bufferSize;
         }

         return System.Text.Encoding.Default.GetString(values);
      }

      /*
      TODO
      public static async Task PopulateCache()
      {
         using var pooledConnection = new DbConnection(DbProviderFactory.ConnectionString);
         await pooledConnection.OpenAsync();

         var (dbCommand, dbDataParameter) = CreateReadCommand(pooledConnection);

         using (dbCommand)
         {
            var cacheKeys = _cacheKeys;
            var cache = _cache;

            for (var i = 1; i < 10001; i++)
            {
               dbDataParameter.Value = i;
               cache.Set<CachedWorld>(cacheKeys[i], await ReadSingleRow(dbCommand));
            }
         }
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

      static async Task<CachedWorld[]> LoadUncachedQueries(int id, int i, int count, CachedWorld[] result)
      {
         using var pooledConnection = new DbConnection(DbProviderFactory.ConnectionString);
         await pooledConnection.OpenAsync();

         var (dbCommand, dbDataParameter) = CreateReadCommand(pooledConnection);

         using (dbCommand)
         {
            Func<ICacheEntry, Task<CachedWorld>> create = async (entry) =>
            {
               return await ReadSingleRow(dbCommand);
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
         }

         return result;
      }
      */
   }
}