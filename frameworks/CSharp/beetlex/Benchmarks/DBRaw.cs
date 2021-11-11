using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Text;
using System.Threading.Tasks;
using System.Collections.Concurrent;
using System.Linq;

namespace Benchmarks
{
    public class RawDb
    {

        private readonly ConcurrentRandom _random;

        private readonly DbProviderFactory _dbProviderFactory;

        public static string _connectionString;

        public RawDb(ConcurrentRandom random, DbProviderFactory dbProviderFactory)
        {
            _random = random;
            _dbProviderFactory = dbProviderFactory;
            OnCreateCommand();
        }

        private void OnCreateCommand()
        {
            SingleCommand = new Npgsql.NpgsqlCommand();
            SingleCommand.CommandText = "SELECT id, randomnumber FROM world WHERE id = @Id";
            var id = SingleCommand.CreateParameter();
            id.ParameterName = "@Id";
            id.DbType = DbType.Int32;
            id.Value = _random.Next(1, 10001);
            SingleCommand.Parameters.Add(id);
            FortuneCommand = new Npgsql.NpgsqlCommand();
            FortuneCommand.CommandText = "SELECT id, message FROM fortune";
        }

        private DbCommand SingleCommand;

        private DbCommand FortuneCommand;

        public async Task<World> LoadSingleQueryRow()
        {
            using (var db = _dbProviderFactory.CreateConnection())
            {
                db.ConnectionString = _connectionString;
                await db.OpenAsync();
                SingleCommand.Connection = db;
                SingleCommand.Parameters[0].Value = _random.Next(1, 10001);
                return await ReadSingleRow(db, SingleCommand);

            }
        }

        async Task<World> ReadSingleRow(DbConnection connection, DbCommand cmd)
        {
            using (var rdr = await cmd.ExecuteReaderAsync(CommandBehavior.SingleRow))
            {
                await rdr.ReadAsync();

                return new World
                {
                    Id = rdr.GetInt32(0),
                    RandomNumber = rdr.GetInt32(1)
                };
            }
        }

        public async Task<World[]> LoadMultipleQueriesRows(int count)
        {
            using (var db = _dbProviderFactory.CreateConnection())
            {
                db.ConnectionString = _connectionString;
                await db.OpenAsync();
                return await LoadMultipleRows(count, db);
            }

        }

        private async Task<World[]> LoadMultipleRows(int count, DbConnection db)
        {
            SingleCommand.Connection = db;
            SingleCommand.Parameters[0].Value = _random.Next(1, 10001);
            var result = new World[count];
            for (int i = 0; i < result.Length; i++)
            {
                result[i] = await ReadSingleRow(db, SingleCommand);
                SingleCommand.Parameters[0].Value = _random.Next(1, 10001);
            }
            return result;

        }

        public async Task<List<Fortune>> LoadFortunesRows()
        {
            var result = new List<Fortune>();

            using (var db = _dbProviderFactory.CreateConnection())

            {
                db.ConnectionString = _connectionString;
                await db.OpenAsync();
                FortuneCommand.Connection = db;
                using (var rdr = await FortuneCommand.ExecuteReaderAsync(CommandBehavior.CloseConnection))
                {
                    while (await rdr.ReadAsync())
                    {
                        result.Add(new Fortune
                        {
                            Id = rdr.GetInt32(0),
                            Message = rdr.GetString(1)
                        });
                    }
                }
            }
            result.Add(new Fortune { Message = "Additional fortune added at request time." });
            result.Sort();
            return result;
        }

        public async Task<World[]> LoadMultipleUpdatesRows(int count)
        {
            using (var db = _dbProviderFactory.CreateConnection())
            {
                db.ConnectionString = _connectionString;
                await db.OpenAsync();

                using (var updateCmd = db.CreateCommand())
                using (var queryCmd = CreateReadCommand(db))
                {
                    var results = new World[count];
                    for (int i = 0; i < count; i++)
                    {
                        results[i] = await ReadSingleRow(db, queryCmd);
                        queryCmd.Parameters["@Id"].Value = _random.Next(1, 10001);
                    }

                    updateCmd.CommandText = BatchUpdateString.Query(count);

                    for (int i = 0; i < count; i++)
                    {
                        var id = updateCmd.CreateParameter();
                        id.ParameterName = $"@Id_{i}";
                        id.DbType = DbType.Int32;
                        updateCmd.Parameters.Add(id);

                        var random = updateCmd.CreateParameter();
                        random.ParameterName = $"@Random_{i}";
                        random.DbType = DbType.Int32;
                        updateCmd.Parameters.Add(random);

                        var randomNumber = _random.Next(1, 10001);
                        id.Value = results[i].Id;
                        random.Value = randomNumber;
                        results[i].RandomNumber = randomNumber;
                    }

                    await updateCmd.ExecuteNonQueryAsync();
                    return results;
                }
            }
        }

        DbCommand CreateReadCommand(DbConnection connection)
        {
            var cmd = connection.CreateCommand();
            cmd.CommandText = "SELECT id, randomnumber FROM world WHERE id = @Id";
            var id = cmd.CreateParameter();
            id.ParameterName = "@Id";
            id.DbType = DbType.Int32;
            id.Value = _random.Next(1, 10001);
            cmd.Parameters.Add(id);
            return cmd;
        }
    }

    internal class BatchUpdateString
    {
        private const int MaxBatch = 500;

        private static string[] _queries = new string[MaxBatch + 1];

        public static string Query(int batchSize)
        {
            if (_queries[batchSize] != null)
            {
                return _queries[batchSize];
            }

            var lastIndex = batchSize - 1;
            var sb = StringBuilderCache.Acquire();
            sb.Append("UPDATE world SET randomNumber = temp.randomNumber FROM (VALUES ");
            Enumerable.Range(0, lastIndex).ToList().ForEach(i => sb.Append($"(@Id_{i}, @Random_{i}), "));
            sb.Append($"(@Id_{lastIndex}, @Random_{lastIndex}) ORDER BY 1) AS temp(id, randomNumber) WHERE temp.id = world.id");
            return _queries[batchSize] = StringBuilderCache.GetStringAndRelease(sb);
        }
    }

    internal static class StringBuilderCache
    {
        private const int DefaultCapacity = 1386;
        private const int MaxBuilderSize = DefaultCapacity * 3;

        [ThreadStatic]
        private static StringBuilder t_cachedInstance;

        /// <summary>Get a StringBuilder for the specified capacity.</summary>
        /// <remarks>If a StringBuilder of an appropriate size is cached, it will be returned and the cache emptied.</remarks>
        public static StringBuilder Acquire(int capacity = DefaultCapacity)
        {
            if (capacity <= MaxBuilderSize)
            {
                StringBuilder sb = t_cachedInstance;
                if (capacity < DefaultCapacity)
                {
                    capacity = DefaultCapacity;
                }

                if (sb != null)
                {
                    // Avoid stringbuilder block fragmentation by getting a new StringBuilder
                    // when the requested size is larger than the current capacity
                    if (capacity <= sb.Capacity)
                    {
                        t_cachedInstance = null;
                        sb.Clear();
                        return sb;
                    }
                }
            }
            return new StringBuilder(capacity);
        }

        public static void Release(StringBuilder sb)
        {
            if (sb.Capacity <= MaxBuilderSize)
            {
                t_cachedInstance = sb;
            }
        }

        public static string GetStringAndRelease(StringBuilder sb)
        {
            string result = sb.ToString();
            Release(sb);
            return result;
        }
    }
}
