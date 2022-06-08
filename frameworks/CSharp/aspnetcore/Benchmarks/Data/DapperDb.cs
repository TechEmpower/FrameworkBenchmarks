// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Data.Common;
using System.Dynamic;
using Benchmarks.Configuration;
using Dapper;
using Microsoft.Extensions.Options;

namespace Benchmarks.Data;

public sealed class DapperDb : IDb
{
    private static readonly Comparison<World> WorldSortComparison = (a, b) => a.Id.CompareTo(b.Id);

    private readonly IRandom _random;
    private readonly DbProviderFactory _dbProviderFactory;
    private readonly string _connectionString;

    public DapperDb(IRandom random, DbProviderFactory dbProviderFactory, IOptions<AppSettings> appSettings)
    {
        _random = random;
        _dbProviderFactory = dbProviderFactory;
        _connectionString = appSettings.Value.ConnectionString;
    }

    public async Task<World> LoadSingleQueryRow()
    {
        using var db = _dbProviderFactory.CreateConnection();
        db.ConnectionString = _connectionString;

        // Note: Don't need to open connection if only doing one thing; let dapper do it
        return await ReadSingleRow(db);
    }

    Task<World> ReadSingleRow(DbConnection db)
    {
        return db.QueryFirstOrDefaultAsync<World>(
                "SELECT id, randomnumber FROM world WHERE id = @Id",
                new { Id = _random.Next(1, 10001) });
    }

    public async Task<World[]> LoadMultipleQueriesRows(int count)
    {
        var results = new World[count];
        using (var db = _dbProviderFactory.CreateConnection())
        {
            db.ConnectionString = _connectionString;
            await db.OpenAsync();

            for (int i = 0; i < count; i++)
            {
                results[i] = await ReadSingleRow(db);
            }
        }

        return results;
    }

    public async Task<World[]> LoadMultipleUpdatesRows(int count)
    {
        IDictionary<string, object> parameters = new ExpandoObject();

        using (var db = _dbProviderFactory.CreateConnection())
        {
            db.ConnectionString = _connectionString;
            await db.OpenAsync();

            var results = new World[count];
            for (int i = 0; i < count; i++)
            {
                results[i] = await ReadSingleRow(db);
            }

            for (int i = 0; i < count; i++)
            {
                var randomNumber = _random.Next(1, 10001);
                parameters[$"@Random_{i}"] = randomNumber;
                parameters[$"@Id_{i}"] = results[i].Id;

                results[i].RandomNumber = randomNumber;
            }

            await db.ExecuteAsync(BatchUpdateString.Query(count), parameters);
            return results;
        }

    }

    public async Task<List<Fortune>> LoadFortunesRows()
    {
        List<Fortune> result;

        using (var db = _dbProviderFactory.CreateConnection())
        {
            db.ConnectionString = _connectionString;

            // Note: don't need to open connection if only doing one thing; let dapper do it
            result = (await db.QueryAsync<Fortune>("SELECT id, message FROM fortune")).AsList();
        }

        result.Add(new Fortune { Message = "Additional fortune added at request time." });
        result.Sort();

        return result;
    }
}
