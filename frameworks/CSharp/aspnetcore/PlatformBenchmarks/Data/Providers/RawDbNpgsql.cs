// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

#if NPGSQL

using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;
using Microsoft.Extensions.Caching.Memory;
using Npgsql;

namespace PlatformBenchmarks
{
    // Is semantically identical to RawDbMySqlConnector.cs.
    // If you are changing RawDbNpgsql.cs, also consider changing RawDbMySqlConnector.cs.
    public class RawDb
    {
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

                var (cmd, _) = CreateReadCommand(db);
                using (cmd)
                {
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

                var (cmd, idParameter) = CreateReadCommand(db);
                using (cmd)
                {
                    for (int i = 0; i < result.Length; i++)
                    {
                        result[i] = await ReadSingleRow(cmd);
                        idParameter.TypedValue = _random.Next(1, 10001);
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

                    var (cmd, idParameter) = rawdb.CreateReadCommand(db);
                    using (cmd)
                    {
                        Func<ICacheEntry, Task<CachedWorld>> create = async (entry) =>
                        {
                            return await rawdb.ReadSingleRow(cmd);
                        };

                        var cacheKeys = _cacheKeys;
                        var key = cacheKeys[id];

                        idParameter.TypedValue = id;

                        for (; i < result.Length; i++)
                        {
                            var data = await rawdb._cache.GetOrCreateAsync<CachedWorld>(key, create);
                            result[i] = data;

                            id = rawdb._random.Next(1, 10001);
                            idParameter.TypedValue = id;
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

                var (cmd, idParameter) = CreateReadCommand(db);
                using (cmd)
                {
                    var cacheKeys = _cacheKeys;
                    var cache = _cache;
                    for (var i = 1; i < 10001; i++)
                    {
                        idParameter.TypedValue = i;
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

                var (queryCmd, queryParameter) = CreateReadCommand(db);
                using (queryCmd)
                {
                    for (int i = 0; i < results.Length; i++)
                    {
                        results[i] = await ReadSingleRow(queryCmd);
                        queryParameter.TypedValue = _random.Next(1, 10001);
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
            var result = new List<Fortune>();

            using (var db = new NpgsqlConnection(_connectionString))
            {
                await db.OpenAsync();

                using (var cmd = new NpgsqlCommand("SELECT id, message FROM fortune", db))
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

        private (NpgsqlCommand readCmd, NpgsqlParameter<int> idParameter) CreateReadCommand(NpgsqlConnection connection)
        {
            var cmd = new NpgsqlCommand("SELECT id, randomnumber FROM world WHERE id = @Id", connection);
            var parameter = new NpgsqlParameter<int>(parameterName: "@Id", value: _random.Next(1, 10001));

            cmd.Parameters.Add(parameter);

            return (cmd, parameter);
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

        private static readonly object[] _cacheKeys = Enumerable.Range(0, 10001).Select((i) => new CacheKey(i)).ToArray();

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

#endif