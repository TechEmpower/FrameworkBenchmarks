using System;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Data;
using System.Data.Common;
using System.Linq;
using System.Runtime.Versioning;
using System.Text;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public class RawDb
    {

        private readonly ConcurrentRandom _random;

        private readonly DbProviderFactory _dbProviderFactory;

        public static string _connectionString = "Server=192.168.2.19;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=256;NoResetOnClose=true;Enlist=false;Max Auto Prepare=3";

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
            using (var db = await DBConnectionGroupPool.Pop())
            {
                SingleCommand.Connection = db.Connection;
                SingleCommand.Parameters[0].Value = _random.Next(1, 10001);
                return await ReadSingleRow(db.Connection, SingleCommand);

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
            using (var db = await DBConnectionGroupPool.Pop())
            {
                return await LoadMultipleRows(count, db.Connection);
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

            using (var db = await DBConnectionGroupPool.Pop())
            {
                FortuneCommand.Connection = db.Connection;
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
            using (var db = await DBConnectionGroupPool.Pop())
            {
                var updateCmd = UpdateCommandsCached.PopCommand(count);
                try
                {
                    updateCmd.Connection = db.Connection;
                    SingleCommand.Connection = db.Connection;
                    SingleCommand.Parameters[0].Value = _random.Next(1, int.MaxValue) % 10000 + 1;
                    var results = new World[count];
                    for (int i = 0; i < count; i++)
                    {
                        results[i] = await ReadSingleRow(db.Connection, SingleCommand);
                        SingleCommand.Parameters[0].Value = _random.Next(1, int.MaxValue) % 10000 + 1;
                    }

                    for (int i = 0; i < count; i++)
                    {
                        var randomNumber = _random.Next(1, int.MaxValue) % 10000 + 1;
                        updateCmd.Parameters[i * 2].Value = results[i].Id;
                        updateCmd.Parameters[i * 2 + 1].Value = randomNumber;
                        results[i].RandomNumber = randomNumber;
                    }

                    await updateCmd.ExecuteNonQueryAsync();
                    return results;
                }
                catch (Exception e_)
                {
                    throw e_;
                }
                finally
                {
                    UpdateCommandsCached.PushCommand(count, updateCmd);
                }
            }
        }
    }

    internal class UpdateCommandsCached
    {
        private static System.Collections.Concurrent.ConcurrentStack<DbCommand>[] mCacheTable
            = new System.Collections.Concurrent.ConcurrentStack<DbCommand>[1024];

        public static string[] IDParamereNames = new string[1024];

        public static string[] RandomParamereNames = new string[1024];

        static UpdateCommandsCached()
        {
            for (int i = 0; i < 1024; i++)
            {
                IDParamereNames[i] = $"@Id_{i}";
                RandomParamereNames[i] = $"@Random_{i}";
                mCacheTable[i] = new System.Collections.Concurrent.ConcurrentStack<DbCommand>();
            }
        }

        private static DbCommand CreatCommand(int count)
        {
            DbCommand cmd = new Npgsql.NpgsqlCommand();
            cmd.CommandText = BatchUpdateString.Query(count);
            for (int i = 0; i < count; i++)
            {
                var id = cmd.CreateParameter();
                id.ParameterName = IDParamereNames[i];
                id.DbType = DbType.Int32;
                cmd.Parameters.Add(id);

                var random = cmd.CreateParameter();
                random.ParameterName = RandomParamereNames[i];
                random.DbType = DbType.Int32;
                cmd.Parameters.Add(random);
            }
            return cmd;

        }

        public static void PushCommand(int count, DbCommand cmd)
        {
            mCacheTable[count].Push(cmd);
        }

        public static DbCommand PopCommand(int count)
        {
            if (mCacheTable[count].TryPop(out DbCommand cmd))
                return cmd;
            return CreatCommand(count);
        }


        public static Task Init()
        {
            for (int i = 1; i <= 500; i++)
            {
                for (int k = 0; k < 10; k++)
                {
                    var cmd = CreatCommand(i);
                    mCacheTable[i].Push(cmd);
                }
            }
            return Task.CompletedTask;
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

    internal class DBConnectionGroupPool
    {
        private static long mIndex;

        private static List<DBconnectionPool> mPools = new List<DBconnectionPool>();

        public static Task Init(int max, string connectionstring)
        {
            int group = 2;
            if (!Program.UpDB)
                group = 16;
            else
                group = 4;
            HttpServer.ApiServer.Log(BeetleX.EventArgs.LogType.Info, null, $"connection pool init group {group}");
            int itemcount = (max / group);
            for (int i = 0; i < group; i++)
            {
                DBconnectionPool pool = new DBconnectionPool();
                pool.Init(itemcount, connectionstring);
                mPools.Add(pool);
            }
            return Task.CompletedTask;
        }

        public static Task<DBConnectionItem> Pop()
        {
            long id = System.Threading.Interlocked.Increment(ref mIndex);
            return mPools[(int)(id % mPools.Count)].Pop();
        }

        public class DBconnectionPool
        {

            private Stack<DBConnectionItem> mConnectionPool = new Stack<DBConnectionItem>();

            private Queue<TaskCompletionSource<DBConnectionItem>> mWaitQueue = new Queue<TaskCompletionSource<DBConnectionItem>>();

            public void Init(int count, string connectionString)
            {
                for (int i = 0; i < count; i++)
                {
                    DbConnection connection = Npgsql.NpgsqlFactory.Instance.CreateConnection();
                    connection.ConnectionString = connectionString;
                    connection.Open();
                    DBConnectionItem item = new DBConnectionItem();
                    item.Pool = this;
                    item.Connection = connection;
                    mConnectionPool.Push(item);
                }
            }


            public Task<DBConnectionItem> Pop()
            {
                lock (this)
                {
                    if (mConnectionPool.Count > 0)
                        return Task.FromResult(mConnectionPool.Pop());
                    TaskCompletionSource<DBConnectionItem> result = new TaskCompletionSource<DBConnectionItem>();
                    mWaitQueue.Enqueue(result);
                    return result.Task;
                }
            }
            public void Push(DBConnectionItem item)
            {
                TaskCompletionSource<DBConnectionItem> work = null;
                lock (this)
                {
                    if (mWaitQueue.Count > 0)
                        work = mWaitQueue.Dequeue();
                    else
                        mConnectionPool.Push(item);
                }
                if (work != null)
                {
                    Task.Run(() => work.SetResult(item));
                }
            }

        }

        public class DBConnectionItem : IDisposable
        {
            public DBconnectionPool Pool { get; set; }

            public DbConnection Connection { get; set; }

            public void Dispose()
            {
                Pool.Push(this);
            }
        }


    }

}
