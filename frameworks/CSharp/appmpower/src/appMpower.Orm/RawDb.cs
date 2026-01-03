using System;
using System.Collections.Generic;
using System.Data;
using System.Threading.Tasks;
using appMpower.Orm.Data; 
using appMpower.Orm.Objects;
using PlatformBenchmarks;

namespace appMpower.Orm
{
   public static class RawDb
   {
      private const int MaxBatch = 500;

#if ADO      
      private static ConcurrentRandom _random = new ConcurrentRandom();
#else
      private static Random _random = new Random();
#endif

      private static string[] _queriesMultipleRows = new string[MaxBatch + 1];

      public static World LoadSingleQueryRow()
      {
         using var pooledConnection = new DbConnection(DbFactory.ConnectionString);
         pooledConnection.Open();

         var (dbCommand, _) = CreateReadCommand(pooledConnection);

         using (dbCommand)
         {
            World world = ReadSingleRow(dbCommand);

            return world;
         }
      }

      public static async Task<World> LoadSingleQueryRowAsync()
      {
         using var pooledConnection = new DbConnection(DbFactory.ConnectionString);
         await pooledConnection.OpenAsync();

         var (dbCommand, _) = CreateReadCommand(pooledConnection);

         using (dbCommand)
         {
            World world = await ReadSingleRowAsync(dbCommand);

            return world;
         }
      }

      public static World LoadSingleQueryRowById(int id)
      {
         using var pooledConnection = new DbConnection(DbFactory.ConnectionString);
         pooledConnection.Open();

         var (dbCommand, _) = CreateReadCommandById(pooledConnection, id);

         using (dbCommand)
         {
            World world = ReadSingleRow(dbCommand);

            return world;
         }
      }

      public static async Task<World> LoadSingleQueryRowByIdAsync(int id)
      {
         using var pooledConnection = new DbConnection(DbFactory.ConnectionString);
         await pooledConnection.OpenAsync();

         var (dbCommand, _) = CreateReadCommandById(pooledConnection, id);

         using (dbCommand)
         {
            World world = await ReadSingleRowAsync(dbCommand);

            return world;
         }
      }

      public static World[] LoadMultipleQueriesRows(int count)
      {
         var worlds = new World[count];

         using var pooledConnection = new DbConnection(DbFactory.ConnectionString);
         pooledConnection.Open();

         var (dbCommand, dbDataParameter) = CreateReadCommand(pooledConnection);

         using (dbCommand)
         {
            for (int i = 0; i < count; i++)
            {
               worlds[i] = ReadSingleRow(dbCommand);
               dbDataParameter.Value = _random.Next(1, 10001);
            }
         }

         return worlds;
      }

      public static async Task<World[]> LoadMultipleQueriesRowsAsync(int count)
      {
         var worlds = new World[count];

         using var pooledConnection = new DbConnection(DbFactory.ConnectionString);
         await pooledConnection.OpenAsync();

         var (dbCommand, dbDataParameter) = CreateReadCommand(pooledConnection);

         using (dbCommand)
         {
            for (int i = 0; i < count; i++)
            {
               worlds[i] = await ReadSingleRowAsync(dbCommand);
               dbDataParameter.Value = _random.Next(1, 10001);
            }
         }

         return worlds;
      }

      public static World[] LoadMultipleUpdatesRows(int count)
      {
         var worlds = new World[count];

         using var pooledConnection = new DbConnection(DbFactory.ConnectionString, true);
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

         using var updateCommand = new DbCommand(BatchUpdateString.Query(count), pooledConnection);

         var ids = BatchUpdateString.Ids;
         var randoms = BatchUpdateString.Randoms;

         for (int i = 0; i < count; i++)
         {
            var randomNumber = _random.Next(1, 10001);

            updateCommand.CreateParameter(ids[i], DbType.Int32, worlds[i].Id);
            updateCommand.CreateParameter(randoms[i], DbType.Int32, randomNumber);

            worlds[i].RandomNumber = randomNumber;
         }

#if !MYSQL
         var jds = BatchUpdateString.Jds;

         for (int i = 0; i < count; i++)
         {
            updateCommand.CreateParameter(jds[i], DbType.Int32, worlds[i].Id);
         }
#endif

         updateCommand.ExecuteNonQuery();

         return worlds;
      }

      public static async Task<World[]> LoadMultipleUpdatesRowsAsync(int count)
      {
         var worlds = new World[count];

         using var pooledConnection = new DbConnection(DbFactory.ConnectionString);
         await pooledConnection.OpenAsync();

         var (queryCommand, dbDataParameter) = CreateReadCommand(pooledConnection);

         using (queryCommand)
         {
            for (int i = 0; i < count; i++)
            {
               worlds[i] = await ReadSingleRowAsync(queryCommand);
               dbDataParameter.Value = _random.Next(1, 10001);
            }
         }

         using var updateCommand = new DbCommand(BatchUpdateString.Query(count), pooledConnection);

         var ids = BatchUpdateString.Ids;
         var randoms = BatchUpdateString.Randoms;

#if !MYSQL
         var jds = BatchUpdateString.Jds;
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

         return worlds;
      }
 
       public static List<Fortune> LoadFortunesRows()
      {
         var fortunes = new List<Fortune>();

         using var pooledConnection = new DbConnection(DbFactory.ConnectionString);
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
                  message: dataReader.GetString(1))
               );
            }

            dataReader.Close();
         }

         fortunes.Add(new Fortune(id: 0, message: "Additional fortune added at request time."));
         fortunes.Sort();

         return fortunes;
      }

      public static async Task<List<Fortune>> LoadFortunesRowsAsync()
      {
         var fortunes = new List<Fortune>();

         using var pooledConnection = new DbConnection(DbFactory.ConnectionString);
         await pooledConnection.OpenAsync();

         var dbCommand = new DbCommand("SELECT * FROM fortune", pooledConnection);

         using (dbCommand)
         {
            var dataReader = await dbCommand.ExecuteReaderAsync(CommandBehavior.SingleResult & CommandBehavior.SequentialAccess);

            while (dataReader.Read())
            {
               fortunes.Add(new Fortune
               (
                  id: dataReader.GetInt32(0),
                  message: dataReader.GetString(1))
               );
            }

            dataReader.Close();
         }

         fortunes.Add(new Fortune(id: 0, message: "Additional fortune added at request time."));
         fortunes.Sort();

         return fortunes;
      }

      private static (DbCommand dbCommand, IDbDataParameter dbDataParameter) CreateReadCommand(DbConnection pooledConnection)
      {
#if ADO         
         var dbCommand = new DbCommand("SELECT * FROM world WHERE id=@Id", pooledConnection);
#else         
         var dbCommand = new DbCommand("SELECT * FROM world WHERE id=?", pooledConnection);
#endif         

         return (dbCommand, dbCommand.CreateParameter("Id", DbType.Int32, _random.Next(1, 10001)));
      }

      internal static (DbCommand dbCommand, IDbDataParameter dbDataParameter) CreateReadCommandById(DbConnection pooledConnection, int id)
      {
#if ADO         
         var dbCommand = new DbCommand("SELECT * FROM world WHERE id=@Id", pooledConnection);
#else         
         var dbCommand = new DbCommand("SELECT * FROM world WHERE id=?", pooledConnection);
#endif         

         return (dbCommand, dbCommand.CreateParameter("Id", DbType.Int32, id));
      }

      internal static World ReadSingleRow(DbCommand dbCommand)
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

      internal static async Task<World> ReadSingleRowAsync(DbCommand dbCommand)
      {
         var dataReader = await dbCommand.ExecuteReaderAsync(CommandBehavior.SingleRow & CommandBehavior.SequentialAccess);

         await dataReader.ReadAsync();

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
         var ids = BatchUpdateString.Ids;
         var worlds = new World[count];
         string queryString;

         if (_queriesMultipleRows[count] != null)
         {
            queryString = _queriesMultipleRows[count];
         }
         else
         {
            var stringBuilder = StringBuilderCache.Acquire();

            for (int i = 0; i < count; i++)
            {
               stringBuilder.Append("SELECT * FROM world WHERE id=?;");
            }

            queryString = _queriesMultipleRows[count] = StringBuilderCache.GetStringAndRelease(stringBuilder);
         }

         using var pooledConnection = new DbConnection(DbFactory.ConnectionString);
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

      public static async Task<World[]> ReadMultipleRowsAsync(int count)
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

         using var pooledConnection = new DbConnection(DbFactory.ConnectionString);
         await pooledConnection.OpenAsync();

         using var dbCommand = new DbCommand(queryString, pooledConnection);

         for (int i = 0; i < count; i++)
         {
            dbCommand.CreateParameter(ids[i], DbType.Int32, _random.Next(1, 10001));
         }

         var dataReader = await dbCommand.ExecuteReaderAsync(CommandBehavior.Default & CommandBehavior.SequentialAccess);

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
   }
}