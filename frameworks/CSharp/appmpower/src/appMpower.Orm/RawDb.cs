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

#if AOTDLL
      public static World LoadSingleQueryRow()
#else
      public static async Task<World> LoadSingleQueryRowAsync()
#endif
      {
         using var pooledConnection = new DbConnection(DbFactory.ConnectionString);
#if AOTDLL
         pooledConnection.Open();
#else
         await pooledConnection.OpenAsync();
#endif

         var (dbCommand, _) = CreateReadCommand(pooledConnection);

         using (dbCommand)
         {
#if AOTDLL
            World world = ReadSingleRow(dbCommand);
#else
            World world = await ReadSingleRowAsync(dbCommand);
#endif
            return world;
         }
      }


#if AOTDLL
      public static World LoadSingleQueryRowById(int id)
#else
      public static async Task<World> LoadSingleQueryRowByIdAsync(int id)
#endif
      {
         using var pooledConnection = new DbConnection(DbFactory.ConnectionString);
#if AOTDLL
         pooledConnection.Open();
#else
         await pooledConnection.OpenAsync();
#endif

         var (dbCommand, _) = CreateReadCommandById(pooledConnection, id);

         using (dbCommand)
         {
#if AOTDLL
            World world = ReadSingleRow(dbCommand);
#else
            World world = await ReadSingleRowAsync(dbCommand);
#endif
            return world;
         }
      }

#if AOTDLL
      public static World[] LoadMultipleQueriesRows(int count)
#else
      public static async Task<World[]> LoadMultipleQueriesRowsAsync(int count)
#endif
      {
         var worlds = new World[count];

         using var pooledConnection = new DbConnection(DbFactory.ConnectionString);
#if AOTDLL
         pooledConnection.Open();
#else
         await pooledConnection.OpenAsync();
#endif

         var (dbCommand, dbDataParameter) = CreateReadCommand(pooledConnection);

         using (dbCommand)
         {
            for (int i = 0; i < count; i++)
            {
#if AOTDLL
               worlds[i] = ReadSingleRow(dbCommand);
#else
               worlds[i] = await ReadSingleRowAsync(dbCommand);
#endif
               dbDataParameter.Value = _random.Next(1, 10001);
            }
         }

         return worlds;
      }

#if AOTDLL
      public static World[] LoadMultipleUpdatesRows(int count)
#else
      public static async Task<World[]> LoadMultipleUpdatesRowsAsync(int count)
#endif
      {
         var worlds = new World[count];

         using var pooledConnection = new DbConnection(DbFactory.ConnectionString, true);
#if AOTDLL
         pooledConnection.Open();
#else
         await pooledConnection.OpenAsync();
#endif

         var (queryCommand, dbDataParameter) = CreateReadCommand(pooledConnection);

         using (queryCommand)
         {
            for (int i = 0; i < count; i++)
            {
#if AOTDLL
               worlds[i] = ReadSingleRow(queryCommand);
#else
               worlds[i] = await ReadSingleRowAsync(queryCommand);
#endif

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
 
#if AOTDLL
      public static List<Fortune> LoadFortunesRows()
#else
      public static async Task<List<Fortune>> LoadFortunesRowsAsync()
#endif
      {
         var fortunes = new List<Fortune>();

         using var pooledConnection = new DbConnection(DbFactory.ConnectionString);
#if AOTDLL
         pooledConnection.Open();
#else
         await pooledConnection.OpenAsync();
#endif

         var dbCommand = new DbCommand("SELECT * FROM fortune", pooledConnection);

         using (dbCommand)
         {
#if AOTDLL
            var dataReader = dbCommand.ExecuteReader(CommandBehavior.SingleResult & CommandBehavior.SequentialAccess);
            while (dataReader.Read())
#else
            var dataReader = await dbCommand.ExecuteReaderAsync(CommandBehavior.SingleResult & CommandBehavior.SequentialAccess);
            while (await dataReader.ReadAsync())
#endif
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

#if AOTDLL
      internal static World ReadSingleRow(DbCommand dbCommand)
#else
      internal static async Task<World> ReadSingleRowAsync(DbCommand dbCommand)
#endif
      {
#if AOTDLL
         var dataReader = dbCommand.ExecuteReader(CommandBehavior.SingleRow & CommandBehavior.SequentialAccess);
         dataReader.Read();
#else
         var dataReader = await dbCommand.ExecuteReaderAsync(CommandBehavior.SingleRow & CommandBehavior.SequentialAccess);
         await dataReader.ReadAsync();
#endif

         var world = new World
         {
            Id = dataReader.GetInt32(0),
            RandomNumber = dataReader.GetInt32(1)
         };

         dataReader.Close();

         return world;
      }

#if AOTDLL
      public static World[] ReadMultipleRows(int count)
#else
      public static async Task<World[]> ReadMultipleRowsAsync(int count)
#endif
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
#if AOTDLL
         pooledConnection.Open();
#else
         await pooledConnection.OpenAsync();
#endif

         using var dbCommand = new DbCommand(queryString, pooledConnection);

         for (int i = 0; i < count; i++)
         {
            dbCommand.CreateParameter(ids[i], DbType.Int32, _random.Next(1, 10001));
         }

#if AOTDLL
            var dataReader = dbCommand.ExecuteReader(CommandBehavior.SingleResult & CommandBehavior.SequentialAccess);
#else
            var dataReader = await dbCommand.ExecuteReaderAsync(CommandBehavior.SingleResult & CommandBehavior.SequentialAccess);
#endif

         do
         {
#if AOTDLL
            dataReader.Read();
#else
            await dataReader.ReadAsync();
#endif

            worlds[j] = new World
            {
               Id = dataReader.GetInt32(0),
               RandomNumber = dataReader.GetInt32(1)
            };

            j++;
         }
#if AOTDLL
         while (dataReader.NextResult());
#else
         while (await dataReader.NextResultAsync());
#endif

         dataReader.Close();

         return worlds;
      }
   }
}