// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;
using Microsoft.Extensions.Caching.Memory;
using Npgsql;

namespace Benchmarks.Data
{
    public interface IRawDb    
    {
        Task<World> LoadSingleQueryRow();

        Task<World[]> LoadMultipleQueriesRows(int count);

        Task<World[]> LoadMultipleUpdatesRows(int count);

        Task<World[]> LoadCachedQueries(int count);

        Task<List<Fortune>> LoadFortunesRows();        
    }


    public class RawDb : IRawDb
    {
        private readonly string _connectionString;
        private readonly MemoryCache _cache;
        
        public RawDb(AppSettings appSettings)
        {
            _connectionString = appSettings.ConnectionString;

            _cache = new MemoryCache(
                new MemoryCacheOptions()
                {
                    ExpirationScanFrequency = TimeSpan.FromMinutes(60)
                });
        }

        public async Task<World> LoadSingleQueryRow()
        {
            using (var db = new NpgsqlConnection(_connectionString))
            {
                await db.OpenAsync();
                
                var (cmd, _) = CreateReadCommand(db, new ConcurrentRandom());
                
                using (cmd)
                {
                    return await ReadSingleRow(cmd);
                }
            }
        }

        public async Task<List<Fortune>> LoadFortunesRows()
        {
            var result = new List<Fortune>();

            using (var db = new NpgsqlConnection(_connectionString))            
            using (var cmd = db.CreateCommand())
            {
                cmd.CommandText = "SELECT id, message FROM fortune";

                await db.OpenAsync();

                using (var rdr = await cmd.ExecuteReaderAsync(CommandBehavior.CloseConnection))
                {
                    while (await rdr.ReadAsync())
                    {
                        result.Add(new Fortune(rdr.GetInt32(0), rdr.GetString(1)));
                    }
                }
            }

            result.Add(new Fortune (0, "Additional fortune added at request time." ));
            result.Sort();

            return result;
        }

        public async Task<World[]> LoadMultipleQueriesRows(int count)
        {
            var random = new ConcurrentRandom();
            var result = new World[count];

            using (var db = new NpgsqlConnection(_connectionString))
            {
                await db.OpenAsync();

                var (cmd, parameter) = CreateReadCommand(db, random);

                using (cmd)
                {
                    for (int i = 0; i < count; i++)
                    {
                        result[i] = await ReadSingleRow(cmd);

                        parameter.Value = random.Next(1, 10001);
                    }
                }
            }

            return result;
        }

        public async Task<World[]> LoadMultipleUpdatesRows(int count)
        {
            var random = new ConcurrentRandom();
            var results = new World[count];

            using (var db = new NpgsqlConnection(_connectionString))
            {
                await db.OpenAsync();

                var (queryCmd, queryParameter) = CreateReadCommand(db, random);

                using (queryCmd)
                {
                    for (int i = 0; i < results.Length; i++)
                    {
                        results[i] = await ReadSingleRow(queryCmd);
                        queryParameter.TypedValue = random.Next(1, 10001);
                    }
                }

                using (var updateCmd = new NpgsqlCommand(BatchUpdateString.Query(count), db))
                {
                    var ids = BatchUpdateString.Ids;
                    var randoms = BatchUpdateString.Randoms;

                    for (int i = 0; i < results.Length; i++)
                    {
                        var randomNumber = random.Next(1, 10001);

                        updateCmd.Parameters.Add(new NpgsqlParameter<int>(parameterName: ids[i], value: results[i].id));
                        updateCmd.Parameters.Add(new NpgsqlParameter<int>(parameterName: randoms[i], value: randomNumber));

                        results[i].randomNumber = randomNumber;
                    }

                    await updateCmd.ExecuteNonQueryAsync();
                }
            }

            return results;
        }

        public Task<World[]> LoadCachedQueries(int count)
        {
            var result = new World[count];
            var cacheKeys = _cacheKeys;
            var cache = _cache;
            var random = new ConcurrentRandom();

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
                    return LoadUncachedQueries(random, id, i, count, this, result);
                }
            }

            return Task.FromResult(result);

            static async Task<World[]> LoadUncachedQueries(ConcurrentRandom random, int id, int i, int count, RawDb rawdb, World[] result)
            {
                using (var db = new NpgsqlConnection(rawdb._connectionString))
                {
                    await db.OpenAsync();

                    var (cmd, idParameter) = rawdb.CreateReadCommand(db,random);
                    
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

                            id = random.Next(1, 10001);
                            idParameter.TypedValue = id;
                            key = cacheKeys[id];
                        }
                    }
                }

                return result;
            }
        }

        private (NpgsqlCommand readCmd, NpgsqlParameter<int> idParameter) CreateReadCommand(NpgsqlConnection connection, ConcurrentRandom random)
        {
            var cmd = new NpgsqlCommand("SELECT id, randomnumber FROM world WHERE id = @Id", connection);

            var parameter = new NpgsqlParameter<int>(parameterName: "@Id", value: random.Next(1, 10001));

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
                    id = rdr.GetInt32(0),
                    randomNumber = rdr.GetInt32(1)
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