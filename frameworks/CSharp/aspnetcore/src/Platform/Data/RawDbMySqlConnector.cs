// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

#if MYSQLCONNECTOR

using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;
using Microsoft.Extensions.Caching.Memory;
using MySqlConnector;

namespace PlatformBenchmarks;

// Is semantically identical to RawDbNpgsql.cs.
// If you are changing RawDbMySqlConnector.cs, also consider changing RawDbNpgsql.cs.
public sealed class RawDb
{
    private readonly string _connectionString;
    private readonly MemoryCache _cache
        = new(new MemoryCacheOptions { ExpirationScanFrequency = TimeSpan.FromMinutes(60) });

    private readonly MySqlDataSource _dataSource;

    public RawDb(AppSettings appSettings)
    {
        _connectionString = appSettings.ConnectionString;
        _dataSource = new MySqlDataSource(appSettings.ConnectionString);
    }

    public async Task<World> LoadSingleQueryRow()
    {
        using var db = await _dataSource.OpenConnectionAsync();
        
        var (cmd, _) = await CreateReadCommandAsync(db);
        using var command = cmd;

        return await ReadSingleRow(cmd);
    }

    public Task<CachedWorld[]> LoadCachedQueries(int count)
    {
        var result = new CachedWorld[count];
        var cacheKeys = _cacheKeys;
        var cache = _cache;
        
        for (var i = 0; i < result.Length; i++)
        {
            var id = Random.Shared.Next(1, 10001);
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
            using var db = await rawdb._dataSource.OpenConnectionAsync();

            var (cmd, idParameter) = await rawdb.CreateReadCommandAsync(db);
            using var command = cmd;
            async Task<CachedWorld> create(ICacheEntry _) => await rawdb.ReadSingleRow(cmd);

            var cacheKeys = _cacheKeys;
            var key = cacheKeys[id];

            idParameter.Value = id;

            for (; i < result.Length; i++)
            {
                result[i] = await rawdb._cache.GetOrCreateAsync(key, create);

                id = Random.Shared.Next(1, 10001);
                idParameter.Value = id;
                key = cacheKeys[id];
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
                var cache = _cache;
                for (var i = 1; i < 10001; i++)
                {
                    idParameter.Value = i;
                    cache.Set<CachedWorld>(cacheKeys[i], await ReadSingleRow(cmd));
                }
            }
        }

        Console.WriteLine("Caching Populated");
    }

    public async Task<World[]> LoadMultipleQueriesRows(int count)
    {
        var results = new World[count];

        using var connection = await _dataSource.OpenConnectionAsync();

        // It is not acceptable to execute multiple SELECTs within a single complex query.
        // It is not acceptable to retrieve all required rows using a SELECT ... WHERE id IN (...) clause.
        // Pipelining of network traffic between the application and database is permitted.
        
        var (queryCmd, queryParameter) = await CreateReadCommandAsync(connection);
        using (queryCmd)
        {
            for (var i = 0; i < results.Length; i++)
            {
                queryParameter.Value = Random.Shared.Next(1, 10001);
                results[i] = await ReadSingleRow(queryCmd);
            }
        }

        return results;
    }

    public async Task<World[]> LoadMultipleUpdatesRows(int count)
    {
        var results = new World[count];

        var ids = new int[count];
        for (var i = 0; i < count; i++)
        {
            ids[i] = Random.Shared.Next(1, 10001);
        }
        Array.Sort(ids);

        using var connection = await _dataSource.OpenConnectionAsync();

        // Each row must be selected randomly using one query in the same fashion as the single database query test
        // Use of IN clauses or similar means to consolidate multiple queries into one operation is not permitted.
        // Similarly, use of a batch or multiple SELECTs within a single statement are not permitted
        var (queryCmd, queryParameter) = await CreateReadCommandAsync(connection);
        using (queryCmd)
        {
            for (var i = 0; i < results.Length; i++)
            {
                queryParameter.Value = ids[i];
                results[i] = await ReadSingleRow(queryCmd);
            }
        }

        // MySql doesn't have the unnest function like PostgreSQL, so we have to do a batch update instead
        using (var updateCmd = new MySqlCommand(BatchUpdateString.Query(count), connection))
        {
            for (var i = 0; i < results.Length; i++)
            {
                var randomNumber = Random.Shared.Next(1, 10001);

                updateCmd.Parameters.AddWithValue($"@Id_{i}", results[i].Id);
                updateCmd.Parameters.AddWithValue($"@Random_{i}", randomNumber);

                results[i].RandomNumber = randomNumber;
            }

            await updateCmd.ExecuteNonQueryAsync();
        }

        return results;
    }

    public async Task<List<FortuneUtf16>> LoadFortunesRows()
    {
        // Benchmark requirements explicitly prohibit pre-initializing the list size
        var result = new List<FortuneUtf16>();

        using var connection = await _dataSource.OpenConnectionAsync();

        using var cmd = new MySqlCommand("SELECT id, message FROM fortune", connection);
        using var rdr = await cmd.ExecuteReaderAsync();

        while (await rdr.ReadAsync())
        {
            result.Add(new FortuneUtf16
            (
                id: rdr.GetInt32(0),
                message: rdr.GetString(1)
            ));
        }

        result.Add(new FortuneUtf16(id: 0, AdditionalFortune));
        result.Sort();

        return result;
    }

    private const string AdditionalFortune = "Additional fortune added at request time.";

    private async Task<(MySqlCommand readCmd, MySqlParameter idParameter)> CreateReadCommandAsync(MySqlConnection connection)
    {
        var cmd = new MySqlCommand("SELECT id, randomnumber FROM world WHERE id = @Id", connection);
        var parameter = new MySqlParameter("@Id", Random.Shared.Next(1, 10001));

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

    private MySqlConnection CreateConnection() => _dataSource.CreateConnection();

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

#endif