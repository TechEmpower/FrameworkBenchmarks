// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System.Buffers.Text;
using System.Data.Common;
using Dapper;
using Minimal.Models;

namespace Minimal.Database;

public class Db
{
    private static readonly Comparison<Fortune> FortuneSortComparison = (a, b) => string.CompareOrdinal(a.Message, b.Message);

    private readonly DbProviderFactory _dbProviderFactory;
    private readonly string _connectionString;

    public Db(AppSettings appSettings)
    {
        ArgumentException.ThrowIfNullOrEmpty(appSettings.ConnectionString);

        _dbProviderFactory = Npgsql.NpgsqlFactory.Instance;
        _connectionString = appSettings.ConnectionString;
    }

    public async Task<World> LoadSingleQueryRow()
    {
        await using var db = _dbProviderFactory.CreateConnection();
        db!.ConnectionString = _connectionString;

        // Note: Don't need to open connection if only doing one thing; let dapper do it
        return await ReadSingleRow(db);
    }

    static Task<World> ReadSingleRow(DbConnection db)
    {
        return db.QueryFirstOrDefaultAsync<World>(
                "SELECT id, randomnumber FROM world WHERE id = @Id",
                new { Id = Random.Shared.Next(1, 10001) });
    }

    public async Task<World[]> LoadMultipleQueriesRows(string? parameter)
    {
        var count = ParseQueries(parameter);

        var results = new World[count];
        await using var db = _dbProviderFactory.CreateConnection();

        db!.ConnectionString = _connectionString;
        await db.OpenAsync();

        for (var i = 0; i < count; i++)
        {
            results[i] = await ReadSingleRow(db);
        }

        return results;
    }

    public async Task<World[]> LoadMultipleUpdatesRows(string? parameter)
    {
        var count = ParseQueries(parameter);

        var parameters = new Dictionary<string, object>();

        await using var db = _dbProviderFactory.CreateConnection();

        db!.ConnectionString = _connectionString;
        await db.OpenAsync();

        var results = new World[count];
        for (var i = 0; i < count; i++)
        {
            results[i] = await ReadSingleRow(db);
        }

        for (var i = 0; i < count; i++)
        {
            var randomNumber = Random.Shared.Next(1, 10001);
            parameters[$"@Rn_{i}"] = randomNumber;
            parameters[$"@Id_{i}"] = results[i].Id;

            results[i].RandomNumber = randomNumber;
        }

        await db.ExecuteAsync(BatchUpdateString.Query(count), parameters);
        return results;
    }

    public async Task<List<Fortune>> LoadFortunesRows()
    {
        List<Fortune> result;

        await using var db = _dbProviderFactory.CreateConnection();

        db!.ConnectionString = _connectionString;

        // Note: don't need to open connection if only doing one thing; let dapper do it
        result = (await db.QueryAsync<Fortune>("SELECT id, message FROM fortune")).AsList();

        result.Add(new Fortune(0, "Additional fortune added at request time."));
        result.Sort(FortuneSortComparison);

        return result;
    }

    private static int ParseQueries(string? parameter)
    {
        if (!int.TryParse(parameter, out int queries))
        {
            queries = 1;
        }
        else
        {
            queries = Math.Clamp(queries, 1, 500);
        }

        return queries;
    }
}