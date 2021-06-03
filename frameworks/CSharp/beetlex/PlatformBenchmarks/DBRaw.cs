using System;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Data;
using System.Data.Common;
using System.Linq;
using System.Runtime.Versioning;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices.ComTypes;
using BeetleX.EventArgs;
using Microsoft.Extensions.Caching.Memory;
using Npgsql;

namespace PlatformBenchmarks
{
    public class RawDb
    {

        private readonly ConcurrentRandom _random;

        private readonly DbProviderFactory _dbProviderFactory;

        private readonly static MemoryCache _cache = new MemoryCache(
          new MemoryCacheOptions()
          {
              ExpirationScanFrequency = TimeSpan.FromMinutes(60)
          });

        private static readonly object[] _cacheKeys = Enumerable.Range(0, 10001).Select((i) => new CacheKey(i)).ToArray();

        public static string _connectionString = null;

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
            mID = new Npgsql.NpgsqlParameter<int>("@Id", _random.Next(1, 10001));
            SingleCommand.Parameters.Add(mID);
            FortuneCommand = new Npgsql.NpgsqlCommand();
            FortuneCommand.CommandText = "SELECT id, message FROM fortune";
        }

        private DbCommand SingleCommand;

        private DbCommand FortuneCommand;

        private Npgsql.NpgsqlParameter<int> mID;

        private static int ListDefaultSize = 8;

        private World[] mWorldBuffer = null;

        private World[] GetWorldBuffer()
        {
            if (mWorldBuffer == null)
                mWorldBuffer = new World[512];
            return mWorldBuffer;
        }

        private Fortune[] mFortunesBuffer = null;

        private Fortune[] GetFortuneBuffer()
        {
            if (mFortunesBuffer == null)
                mFortunesBuffer = new Fortune[512];
            return mFortunesBuffer;
        }

        public async Task<World> LoadSingleQueryRow()
        {
            using (var db = _dbProviderFactory.CreateConnection())
            {
                db.ConnectionString = _connectionString;
                await db.OpenAsync();
                SingleCommand.Connection = db;
                mID.TypedValue = _random.Next(1, 10001);
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

        public async Task<ArraySegment<World>> LoadMultipleQueriesRows(int count)
        {
            using (var db = _dbProviderFactory.CreateConnection())
            {
                db.ConnectionString = _connectionString;
                await db.OpenAsync();
                return await LoadMultipleRows(count, db);
            }
        }


        public Task<World[]> LoadCachedQueries(int count)
        {
            var result = new World[count];
            var cacheKeys = _cacheKeys;
            var cache = _cache;
            var random = _random;
            for (var i = 0; i < result.Length; i++)
            {
                var id = random.Next(1, 10001);
                var key = cacheKeys[id];
                var data = cache.Get<CachedWorld>(key);

                if (data != null)
                {
                    result[i] = data;
                }
                else
                {
                    return LoadUncachedQueries(id, i, count, this, result);
                }
            }

            return Task.FromResult(result);

            static async Task<World[]> LoadUncachedQueries(int id, int i, int count, RawDb rawdb, World[] result)
            {
                using (var db = new NpgsqlConnection(_connectionString))
                {
                    await db.OpenAsync();
                    Func<ICacheEntry, Task<CachedWorld>> create = async (entry) =>
                    {
                        return await rawdb.ReadSingleRow(db, rawdb.SingleCommand);
                    };

                    var cacheKeys = _cacheKeys;
                    var key = cacheKeys[id];

                    rawdb.SingleCommand.Connection = db;
                    rawdb.mID.TypedValue = id;
                    for (; i < result.Length; i++)
                    {
                        var data = await _cache.GetOrCreateAsync<CachedWorld>(key, create);
                        result[i] = data;
                        id = rawdb._random.Next(1, 10001);
                        rawdb.SingleCommand.Connection = db;
                        rawdb.mID.TypedValue = id;
                        key = cacheKeys[id];
                    }
                }
                return result;
            }
        }


        private async Task<ArraySegment<World>> LoadMultipleRows(int count, DbConnection db)
        {
            SingleCommand.Connection = db;
            SingleCommand.Parameters[0].Value = _random.Next(1, 10001);
            var result = GetWorldBuffer();
            for (int i = 0; i < count; i++)
            {
                result[i] = await ReadSingleRow(db, SingleCommand);
                SingleCommand.Parameters[0].Value = _random.Next(1, 10001);
            }
            return new ArraySegment<World>(result, 0, count);

        }

        public async Task<ArraySegment<Fortune>> LoadFortunesRows()
        {
            int count = 0;
            var result = GetFortuneBuffer();
            using (var db = new NpgsqlConnection(_connectionString))
            {
                await db.OpenAsync();
                FortuneCommand.Connection = db;
                using (var rdr = await FortuneCommand.ExecuteReaderAsync())
                {
                    while (await rdr.ReadAsync())
                    {
                        result[count] = (new Fortune
                        {
                            Id = rdr.GetInt32(0),
                            Message = rdr.GetString(1)
                        });
                        count++;
                    }
                }
            }
            result[count] = (new Fortune { Message = "Additional fortune added at request time." });
            count++;
            Array.Sort<Fortune>(result, 0, count);
            return new ArraySegment<Fortune>(result, 0, count);
        }
        public async Task<World[]> LoadMultipleUpdatesRows(int count)
        {
            using (var db = new NpgsqlConnection(_connectionString))
            {
                await db.OpenAsync();
                var updateCmd = UpdateCommandsCached.PopCommand(count);
                try
                {
                    var command = updateCmd.Command;
                    command.Connection = db;
                    SingleCommand.Connection = db;
                    mID.TypedValue = _random.Next(1, int.MaxValue) % 10000 + 1;
                    var results = new World[count];
                    for (int i = 0; i < count; i++)
                    {
                        results[i] = await ReadSingleRow(db, SingleCommand);
                        mID.TypedValue = _random.Next(1, int.MaxValue) % 10000 + 1;
                    }

                    for (int i = 0; i < count; i++)
                    {
                        var randomNumber = _random.Next(1, int.MaxValue) % 10000 + 1;
                        updateCmd.Parameters[i * 2].TypedValue = results[i].Id;
                        updateCmd.Parameters[i * 2 + 1].TypedValue = randomNumber;
                        //updateCmd.Parameters[i * 2].Value = results[i].Id;
                        //updateCmd.Parameters[i * 2 + 1].Value = randomNumber;
                        results[i].RandomNumber = randomNumber;
                    }

                    await command.ExecuteNonQueryAsync();
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


    public sealed class CacheKey : IEquatable<CacheKey>
    {
        private readonly int _value;

        public CacheKey(int value)
            => _value = value;

        public bool Equals(CacheKey key)
            => key._value == _value;

        public override bool Equals(object obj)
            => ReferenceEquals(obj, this);

        public override int GetHashCode()
            => _value;

        public override string ToString()
            => _value.ToString();
    }

    internal class UpdateCommandsCached
    {
        private static System.Collections.Concurrent.ConcurrentStack<CommandCacheItem>[] mCacheTable
            = new System.Collections.Concurrent.ConcurrentStack<CommandCacheItem>[1024];

        public static string[] IDParamereNames = new string[1024];

        public static string[] RandomParamereNames = new string[1024];

        static UpdateCommandsCached()
        {
            for (int i = 0; i < 1024; i++)
            {
                IDParamereNames[i] = $"@Id_{i}";
                RandomParamereNames[i] = $"@Random_{i}";
                mCacheTable[i] = new System.Collections.Concurrent.ConcurrentStack<CommandCacheItem>();
            }
        }

        private static CommandCacheItem CreatCommand(int count)
        {
            CommandCacheItem item = new CommandCacheItem();
            NpgsqlCommand cmd = new Npgsql.NpgsqlCommand();
            cmd.CommandText = BatchUpdateString.Query(count);
            for (int i = 0; i < count; i++)
            {
                var id = new NpgsqlParameter<int>();
                id.ParameterName = IDParamereNames[i];
                cmd.Parameters.Add(id);
                item.Parameters.Add(id);

                var random = new NpgsqlParameter<int>();
                random.ParameterName = RandomParamereNames[i];
                cmd.Parameters.Add(random);
                item.Parameters.Add(random);
            }
            item.Command = cmd;
            return item;

        }

        public static void PushCommand(int count, CommandCacheItem cmd)
        {
            mCacheTable[count].Push(cmd);
        }

        public static CommandCacheItem PopCommand(int count)
        {
            if (mCacheTable[count].TryPop(out CommandCacheItem cmd))
                return cmd;
            return CreatCommand(count);
        }

        private static bool mInited = false;

        public static void Init()
        {
            if (mInited)
                return;
            lock (typeof(UpdateCommandsCached))
            {
                if (mInited)
                    return;
                for (int i = 1; i <= 500; i++)
                {
                    for (int k = 0; k < 10; k++)
                    {
                        var cmd = CreatCommand(i);
                        mCacheTable[i].Push(cmd);
                    }
                }
                mInited = true;
                HttpServer.ApiServer.Log(LogType.Info, null, $"Init update commands cached");
                return;
            }
        }
    }

    class CommandCacheItem
    {
        public Npgsql.NpgsqlCommand Command { get; set; }

        public List<Npgsql.NpgsqlParameter<int>> Parameters { get; private set; } = new List<Npgsql.NpgsqlParameter<int>>(1024);
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
