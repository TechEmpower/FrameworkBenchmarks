// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;
using Microsoft.Extensions.Caching.Memory;
using Npgsql;

namespace PlatformBenchmarks
{
    public class RawDb
    {
        private static readonly ConcurrentQueue<SqlReadCommand> _sqlReadCommands = new ConcurrentQueue<SqlReadCommand>();
        private static readonly ConcurrentQueue<SqlFortuneCommand> _sqlFortuneCommands = new ConcurrentQueue<SqlFortuneCommand>();
        private static readonly object[] _cacheKeys = Enumerable.Range(0, 10001).Select((i) => new CacheKey(i)).ToArray();
        private readonly ConcurrentRandom _random;
        private readonly string _connectionString;
        private readonly MemoryCache _cache = new MemoryCache(
            new MemoryCacheOptions()
            {
                ExpirationScanFrequency = TimeSpan.FromMinutes(60)
            });

        public RawDb(ConcurrentRandom random, AppSettings appSettings)
        {
            _random = random;
            _connectionString = appSettings.ConnectionString;
        }

        public async Task<World> LoadSingleQueryRow()
        {
            using (var db = new NpgsqlConnection(_connectionString))
            {
                await db.OpenAsync();

                using (var cmd = CreateReadCommand())
                {
                    cmd.Connection = db;
                    cmd.Parameter.TypedValue = _random.Next(0, 10000) + 1;
                    return await ReadSingleRow(cmd);
                }
            }
        }

        public async Task<World[]> LoadMultipleQueriesRows(int count)
        {
            var result = new World[count];

            using (var db = new NpgsqlConnection(_connectionString))
            {
                await db.OpenAsync();

                using (var cmd = CreateReadCommand())
                {
                    cmd.Connection = db;
                    var param = cmd.Parameter;
                    for (int i = 0; i < result.Length; i++)
                    {
                        param.TypedValue = _random.Next(0, 10000) + 1;
                        result[i] = await ReadSingleRow(cmd);
                    }
                }
            }

            return result;
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
                using (var db = new NpgsqlConnection(rawdb._connectionString))
                {
                    await db.OpenAsync();

                    using (var cmd = CreateReadCommand())
                    {
                        cmd.Connection = db;
                        Func<ICacheEntry, Task<CachedWorld>> create = async (entry) => 
                        {
                            return await rawdb.ReadSingleRow(cmd);
                        };

                        var cacheKeys = _cacheKeys;
                        var key = cacheKeys[id];

                        var param = cmd.Parameter;
                        param.TypedValue = id;

                        for (; i < result.Length; i++)
                        {
                            var data = await rawdb._cache.GetOrCreateAsync<CachedWorld>(key, create);
                            result[i] = data;

                            id = rawdb._random.Next(1, 10001);
                            param.TypedValue = id;
                            key = cacheKeys[id];
                        }
                    }
                }

                return result;
            }
        }

        public async Task PopulateCache()
        {
            using (var db = new NpgsqlConnection(_connectionString))
            {
                await db.OpenAsync();

                using (var cmd = CreateReadCommand())
                {
                    cmd.Connection = db;
                    var cacheKeys = _cacheKeys;
                    var cache = _cache;
                    for (var i = 1; i < 10001; i++)
                    {
                        cmd.Parameter.TypedValue = i;
                        cache.Set<CachedWorld>(cacheKeys[i], await ReadSingleRow(cmd));
                    }
                }
            }

            Console.WriteLine("Caching Populated");
        }

        public async Task<World[]> LoadMultipleUpdatesRows(int count)
        {
            var results = new World[count];

            using (var db = new NpgsqlConnection(_connectionString))
            {
                await db.OpenAsync();

                using (var cmd = CreateReadCommand())
                {
                    cmd.Connection = db;
                    var queryParameter = cmd.Parameter;
                    for (int i = 0; i < results.Length; i++)
                    {
                        queryParameter.TypedValue = _random.Next(0, 10000) + 1;
                        results[i] = await ReadSingleRow(cmd);
                    }
                }

                using (var updateCmd = new NpgsqlCommand(BatchUpdateString.Query(count), db))
                {
                    var ids = BatchUpdateString.Ids;
                    var randoms = BatchUpdateString.Randoms;

                    for (int i = 0; i < results.Length; i++)
                    {
                        var randomNumber = _random.Next(1, 10001);

                        updateCmd.Parameters.Add(new NpgsqlParameter<int>(parameterName: ids[i], value: results[i].Id));
                        updateCmd.Parameters.Add(new NpgsqlParameter<int>(parameterName: randoms[i], value: randomNumber));

                        results[i].RandomNumber = randomNumber;
                    }

                    await updateCmd.ExecuteNonQueryAsync();
                }
            }

            return results;
        }

        public async Task<List<Fortune>> LoadFortunesRows()
        {
            var result = new List<Fortune>(20);

            using (var db = new NpgsqlConnection(_connectionString))
            {
                await db.OpenAsync();

                using var cmd = CreateFortuneCommand();
                cmd.Connection = db;

                using (var rdr = await cmd.ExecuteReaderAsync())
                {
                    while (await rdr.ReadAsync())
                    {
                        result.Add(new Fortune
                        (
                            id:rdr.GetInt32(0),
                            message: rdr.GetString(1)
                        ));
                    }
                }
            }

            result.Add(new Fortune(id: 0, message: "Additional fortune added at request time." ));
            result.Sort();

            return result;
        }

        private static SqlReadCommand CreateReadCommand()
        {
            if (!_sqlReadCommands.TryDequeue(out var cmd))
            {
                cmd = new SqlReadCommand();
            }

            return cmd;
        }

        private static SqlFortuneCommand CreateFortuneCommand()
        {
            if (!_sqlFortuneCommands.TryDequeue(out var cmd))
            {
                cmd = new SqlFortuneCommand();
            }

            return cmd;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private async Task<World> ReadSingleRow(NpgsqlCommand cmd)
        {
            using (var rdr = await cmd.ExecuteReaderAsync(System.Data.CommandBehavior.SingleRow))
            {
                await rdr.ReadAsync();

                return new World
                {
                    Id = rdr.GetInt32(0),
                    RandomNumber = rdr.GetInt32(1)
                };
            }
        }

        internal class SqlFortuneCommand : IDisposable
        {
            private readonly NpgsqlCommand _cmd;

            public NpgsqlConnection Connection
            {
                set { _cmd.Connection = value; }
            }

            public SqlFortuneCommand()
            {
                _cmd = new NpgsqlCommand("SELECT id, message FROM fortune");
            }

            public Task<NpgsqlDataReader> ExecuteReaderAsync() => _cmd.ExecuteReaderAsync();

            public void Dispose()
            {
                _cmd.Connection = null;
                _sqlFortuneCommands.Enqueue(this);
            }
        }

        internal class SqlReadCommand : IDisposable
        {
            private readonly NpgsqlCommand _cmd;
            private readonly NpgsqlParameter<int> _parameter;

            public NpgsqlParameter<int> Parameter => _parameter;
            public NpgsqlConnection Connection
            {
                set { _cmd.Connection = value; }
            }

            public SqlReadCommand()
            {
                _cmd = new NpgsqlCommand("SELECT id, randomnumber FROM world WHERE id = @Id");
                _parameter = new NpgsqlParameter<int>(parameterName: "@Id", value: 0);
                _cmd.Parameters.Add(_parameter);
            }

            public static implicit operator NpgsqlCommand(SqlReadCommand c) => c._cmd;

            public void Dispose()
            {
                _cmd.Connection = null;
                _sqlReadCommands.Enqueue(this);
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
    }
}
