// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

#if MYSQLCONNECTOR

using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;
using FastCache;
using FastCache.Extensions;
using Microsoft.Extensions.Caching.Memory;
using MySqlConnector;

namespace PlatformBenchmarks
{
    // Is semantically identical to RawDbNpgsql.cs.
    // If you are changing RawDbMySqlConnector.cs, also consider changing RawDbNpgsql.cs.
    public class RawDb
    {
        private readonly ConcurrentRandom _random;
        private readonly string _connectionString;

        public RawDb(ConcurrentRandom random, AppSettings appSettings)
        {
            _random = random;
            _connectionString = appSettings.ConnectionString;
        }

        public async Task<World> LoadSingleQueryRow()
        {
            using (var db = new MySqlConnection(_connectionString))
            {
                await db.OpenAsync();

                var (cmd, _) = await CreateReadCommandAsync(db);
                using (cmd)
                {
                    return await ReadSingleRow(cmd);
                }
            }
        }

        public async Task<World[]> LoadMultipleQueriesRows(int count)
        {
            var result = new World[count];

            using (var db = new MySqlConnection(_connectionString))
            {
                await db.OpenAsync();

                var (cmd, idParameter) = await CreateReadCommandAsync(db);
                using (cmd)
                {
                    for (int i = 0; i < result.Length; i++)
                    {
                        result[i] = await ReadSingleRow(cmd);
                        idParameter.Value = _random.Next(1, 10001);
                    }
                }
            }

            return result;
        }

        public Task<CachedWorld[]> LoadCachedQueries(int count)
        {
            var result = new CachedWorld[count];
            var cacheKeys = _cacheKeys;
            var random = _random;
            for (var i = 0; i < result.Length; i++)
            {
                var id = random.Next(1, 10001);
                var key = cacheKeys[id];
                if (Cached<CachedWorld>.TryGet(key, out var cached))
                {
                    result[i] = cached.Value;
                }
                else
                {
                    return LoadUncachedQueries(id, i, count, this, result);
                }
            }

            return Task.FromResult(result);

            async Task<CachedWorld[]> LoadUncachedQueries(int id, int i, int count, RawDb rawdb, CachedWorld[] result)
            {
                using (var db = new MySqlConnection(rawdb._connectionString))
                {
                    await db.OpenAsync();

                    var (cmd, idParameter) = await rawdb.CreateReadCommandAsync(db);
                    using (cmd)
                    {
                        async Task<CachedWorld> create(int key) => await ReadSingleRow(cmd);

                        var cacheKeys = _cacheKeys;
                        var key = cacheKeys[id];

                        idParameter.Value = id;

                        for (; i < result.Length; i++)
                        {
                            result[i] = await Cached.GetOrCompute(key, create, TimeSpan.FromMinutes(60));

                            id = rawdb._random.Next(1, 10001);
                            idParameter.Value = id;
                            key = cacheKeys[id];
                        }
                    }
                }

                return result;
            }
        }

        public async Task PopulateCache()
        {
            using (var db = new MySqlConnection(_connectionString))
            {
                await db.OpenAsync();

                var (cmd, idParameter) = await CreateReadCommandAsync(db);
                using (cmd)
                {
                    var cacheKeys = _cacheKeys;
                    for (var i = 1; i < 10001; i++)
                    {
                        idParameter.Value = i;
                        await ReadSingleRow(cmd).Cache(cacheKeys[i], TimeSpan.FromMinutes(60));
                    }
                }
            }

            Console.WriteLine("Caching Populated");
        }

        public async Task<World[]> LoadMultipleUpdatesRows(int count)
        {
            var results = new World[count];

            using (var db = new MySqlConnection(_connectionString))
            {
                await db.OpenAsync();

                var (queryCmd, queryParameter) = await CreateReadCommandAsync(db);
                using (queryCmd)
                {
                    for (int i = 0; i < results.Length; i++)
                    {
                        results[i] = await ReadSingleRow(queryCmd);
                        queryParameter.Value = _random.Next(1, 10001);
                    }
                }

                using (var updateCmd = new MySqlCommand(BatchUpdateString.Query(count), db))
                {
                    var ids = BatchUpdateString.Ids;
                    var randoms = BatchUpdateString.Randoms;

                    for (int i = 0; i < results.Length; i++)
                    {
                        var randomNumber = _random.Next(1, 10001);

                        updateCmd.Parameters.Add(new MySqlParameter(ids[i], results[i].Id));
                        updateCmd.Parameters.Add(new MySqlParameter(randoms[i], randomNumber));

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

            using (var db = new MySqlConnection(_connectionString))
            {
                await db.OpenAsync();

                using (var cmd = new MySqlCommand("SELECT id, message FROM fortune", db))
                {
                    await cmd.PrepareAsync();
                    
                    using (var rdr = await cmd.ExecuteReaderAsync())
                    {
                        while (await rdr.ReadAsync())
                        {
                            result.Add(
                                new Fortune
                                (
                                    id: rdr.GetInt32(0),
                                    message: rdr.GetString(1)
                                ));
                        }
                    }
                }
            }

            result.Add(new Fortune(id: 0, message: "Additional fortune added at request time." ));
            result.Sort();

            return result;
        }

        private async Task<(MySqlCommand readCmd, MySqlParameter idParameter)> CreateReadCommandAsync(MySqlConnection connection)
        {
            var cmd = new MySqlCommand("SELECT id, randomnumber FROM world WHERE id = @Id", connection);
            var parameter = new MySqlParameter("@Id", _random.Next(1, 10001));

            cmd.Parameters.Add(parameter);

            await cmd.PrepareAsync();
            
            return (cmd, parameter);
        }
        
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private async Task<World> ReadSingleRow(MySqlCommand cmd)
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
        
        private static readonly int[] _cacheKeys = Enumerable.Range(0, 10001).ToArray();
    }
}

#endif