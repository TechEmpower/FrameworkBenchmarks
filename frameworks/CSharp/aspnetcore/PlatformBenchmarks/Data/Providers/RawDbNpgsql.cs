// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

#if NPGSQL

using System.Runtime.CompilerServices;
using Microsoft.Extensions.Caching.Memory;
using Npgsql;

namespace PlatformBenchmarks
{
    // Is semantically identical to RawDbMySqlConnector.cs.
    // If you are changing RawDbNpgsql.cs, also consider changing RawDbMySqlConnector.cs.
    public sealed class RawDb
    {
        private readonly ConcurrentRandom _random;
        private readonly MemoryCache _cache = new(
            new MemoryCacheOptions
            {
                ExpirationScanFrequency = TimeSpan.FromMinutes(60)
            });

        private readonly NpgsqlDataSource _dataSource;

        public RawDb(ConcurrentRandom random, AppSettings appSettings)
        {
            _random = random;
            _dataSource = NpgsqlDataSource.Create(appSettings.ConnectionString);
        }

        public async Task<World> LoadSingleQueryRow()
        {
            using var db = _dataSource.CreateConnection();
            await db.OpenAsync();

            var (cmd, _) = CreateReadCommand(db);
            using var command = cmd;
            return await ReadSingleRow(cmd);
        }

        public async Task<World[]> LoadMultipleQueriesRows(int count)
        {
            var results = new World[count];

            using var connection = await _dataSource.OpenConnectionAsync();

            using var batch = new NpgsqlBatch(connection)
            {
                // Inserts a PG Sync message between each statement in the batch, required for compliance with
                // TechEmpower general test requirement 7
                // https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview
                EnableErrorBarriers = true
            };

            for (var i = 0; i < count; i++)
            {
                batch.BatchCommands.Add(new()
                {
                    CommandText = "SELECT id, randomnumber FROM world WHERE id = $1",
                    Parameters = { new NpgsqlParameter<int> { TypedValue = _random.Next(1, 10001) } }
                });
            }

            using var reader = await batch.ExecuteReaderAsync();

            for (var i = 0; i < count; i++)
            {
                await reader.ReadAsync();
                results[i] = new World { Id = reader.GetInt32(0), RandomNumber = reader.GetInt32(1) };
                await reader.NextResultAsync();
            }

            return results;
        }

        public Task<CachedWorld[]> LoadCachedQueries(int count)
        {
            var result = new CachedWorld[count];
            var cacheKeys = _cacheKeys;
            var cache = _cache;
            var random = _random;
            for (var i = 0; i < result.Length; i++)
            {
                var id = random.Next(1, 10001);
                var key = cacheKeys[id];
                if (cache.TryGetValue(key, out var cached))
                {
                    result[i] = (CachedWorld)cached;
                }
                else
                {
                    return LoadUncachedQueries(id, i, count, this, result);
                }
            }

            return Task.FromResult(result);

            static async Task<CachedWorld[]> LoadUncachedQueries(int id, int i, int count, RawDb rawdb, CachedWorld[] result)
            {
                using var db = rawdb._dataSource.CreateConnection();
                await db.OpenAsync();

                var (cmd, idParameter) = rawdb.CreateReadCommand(db);
                using var command = cmd;
                async Task<CachedWorld> create(ICacheEntry _) => await ReadSingleRow(cmd);

                var cacheKeys = _cacheKeys;
                var key = cacheKeys[id];

                idParameter.TypedValue = id;

                for (; i < result.Length; i++)
                {
                    result[i] = await rawdb._cache.GetOrCreateAsync(key, create);

                    id = rawdb._random.Next(1, 10001);
                    idParameter.TypedValue = id;
                    key = cacheKeys[id];
                }

                return result;
            }
        }

        public async Task PopulateCache()
        {
            using var db = _dataSource.CreateConnection();
            await db.OpenAsync();

            var (cmd, idParameter) = CreateReadCommand(db);
            using var command = cmd;
            var cacheKeys = _cacheKeys;
            var cache = _cache;
            for (var i = 1; i < 10001; i++)
            {
                idParameter.TypedValue = i;
                cache.Set<CachedWorld>(cacheKeys[i], await ReadSingleRow(cmd));
            }

            Console.WriteLine("Caching Populated");
        }

        public async Task<World[]> LoadMultipleUpdatesRows(int count)
        {
            var results = new World[count];

            using var connection = _dataSource.CreateConnection();
            await connection.OpenAsync();

            using (var batch = new NpgsqlBatch(connection))
            {
                // Inserts a PG Sync message between each statement in the batch, required for compliance with
                // TechEmpower general test requirement 7
                // https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview
                batch.EnableErrorBarriers = true;

                for (var i = 0; i < count; i++)
                {
                    batch.BatchCommands.Add(new()
                    {
                        CommandText = "SELECT id, randomnumber FROM world WHERE id = $1",
                        Parameters = { new NpgsqlParameter<int> { TypedValue = _random.Next(1, 10001) } }
                    });
                }

                using var reader = await batch.ExecuteReaderAsync();

                for (var i = 0; i < count; i++)
                {
                    await reader.ReadAsync();
                    results[i] = new World { Id = reader.GetInt32(0), RandomNumber = reader.GetInt32(1) };
                    await reader.NextResultAsync();
                }
            }

            using (var updateCmd = new NpgsqlCommand(BatchUpdateString.Query(count), connection))
            {
                for (int i = 0; i < results.Length; i++)
                {
                    var randomNumber = _random.Next(1, 10001);

                    updateCmd.Parameters.Add(new NpgsqlParameter<int> { TypedValue = results[i].Id });
                    updateCmd.Parameters.Add(new NpgsqlParameter<int> { TypedValue = randomNumber });

                    results[i].RandomNumber = randomNumber;
                }

                await updateCmd.ExecuteNonQueryAsync();
            }

            return results;
        }

        public async Task<List<FortuneUtf8>> LoadFortunesRows()
        {
            var result = new List<FortuneUtf8>();

            using (var db = _dataSource.CreateConnection())
            {
                await db.OpenAsync();

                using var cmd = new NpgsqlCommand("SELECT id, message FROM fortune", db);
                using var rdr = await cmd.ExecuteReaderAsync();
                while (await rdr.ReadAsync())
                {
                    result.Add(new FortuneUtf8
                    (
                        id:rdr.GetInt32(0),
                        message: rdr.GetFieldValue<byte[]>(1)
                    ));
                }
            }

            result.Add(new FortuneUtf8(id: 0, AdditionalFortune));
            result.Sort();

            return result;
        }

        private readonly byte[] AdditionalFortune = "Additional fortune added at request time."u8.ToArray();

        private (NpgsqlCommand readCmd, NpgsqlParameter<int> idParameter) CreateReadCommand(NpgsqlConnection connection)
        {
            var cmd = new NpgsqlCommand("SELECT id, randomnumber FROM world WHERE id = $1", connection);
            var parameter = new NpgsqlParameter<int> { TypedValue = _random.Next(1, 10001) };

            cmd.Parameters.Add(parameter);

            return (cmd, parameter);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static async Task<World> ReadSingleRow(NpgsqlCommand cmd)
        {
            using var rdr = await cmd.ExecuteReaderAsync(System.Data.CommandBehavior.SingleRow);
            await rdr.ReadAsync();

            return new World
            {
                Id = rdr.GetInt32(0),
                RandomNumber = rdr.GetInt32(1)
            };
        }

        private static readonly object[] _cacheKeys = Enumerable.Range(0, 10001).Select(i => new CacheKey(i)).ToArray();

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
