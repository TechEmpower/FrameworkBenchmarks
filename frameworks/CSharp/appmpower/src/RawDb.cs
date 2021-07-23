using System;
using System.Collections.Generic;
using System.Data;
using System.Text;
using System.Threading.Tasks;
using appMpower.Db;

namespace appMpower
{
   public static class RawDb
   {
      private const int MaxBatch = 500;
      private static Random _random = new Random();
      private static string[] _queriesMultipleRows = new string[MaxBatch + 1];

      public static async Task<World> LoadSingleQueryRow()
      {
         var pooledConnection = await PooledConnections.GetConnection(ConnectionStrings.OdbcConnection);
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

         var pooledConnection = await PooledConnections.GetConnection(ConnectionStrings.OdbcConnection);
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

         var pooledConnection = await PooledConnections.GetConnection(ConnectionStrings.OdbcConnection);
         pooledConnection.Open();

         var pooledCommand = new PooledCommand("SELECT * FROM fortune", pooledConnection);
         var dataReader = await pooledCommand.ExecuteReaderAsync(CommandBehavior.SingleResult);

         while (dataReader.Read())
         {
            fortunes.Add(new Fortune
            (
                id: dataReader.GetInt32(0),
                message: dataReader.GetString(1)
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

         var pooledConnection = await PooledConnections.GetConnection(ConnectionStrings.OdbcConnection);
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
         // --- only for alternative update statement - will be used for MySQL
         //var jds = PlatformBenchmarks.BatchUpdateString.Jds;

         for (int i = 0; i < count; i++)
         {
            var randomNumber = _random.Next(1, 10001);

            updateCommand.CreateParameter(ids[i], DbType.Int32, worlds[i].Id);
            updateCommand.CreateParameter(randoms[i], DbType.Int32, randomNumber);

            worlds[i].RandomNumber = randomNumber;
         }

         // --- only for alternative update statement - will be used for MySQL
         //for (int i = 0; i < count; i++)
         //{
         //   updateCommand.CreateParameter(jds[i], DbType.Int32, worlds[i].Id);
         //}

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
         var dataReader = await pooledCommand.ExecuteReaderAsync(CommandBehavior.SingleRow);

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

         var pooledConnection = await PooledConnections.GetConnection(ConnectionStrings.OdbcConnection);
         pooledConnection.Open();

         var pooledCommand = new PooledCommand(queryString, pooledConnection);

         for (int i = 0; i < count; i++)
         {
            pooledCommand.CreateParameter(ids[i], DbType.Int32, _random.Next(1, 10001));
         }

         var dataReader = await pooledCommand.ExecuteReaderAsync(CommandBehavior.Default);

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
   }
}