// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Dynamic;
using System.Text;
using System.Threading.Tasks;
using Benchmarks.Configuration;
using Dapper;
using Microsoft.Extensions.Options;

namespace Benchmarks.Data
{
    public class DapperDb : IDb
    {
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
            using (var db = _dbProviderFactory.CreateConnection())
            {
                db.ConnectionString = _connectionString;

                // Note: Don't need to open connection if only doing one thing; let dapper do it
                return await ReadSingleRow(db);
            }
        }

        async Task<World> ReadSingleRow(DbConnection db)
        {
            return await db.QueryFirstOrDefaultAsync<World>(
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
            var results = new World[count];
            IDictionary<string, object> parameters = new ExpandoObject();
            var updateCommand = new StringBuilder(count);

            using (var db = _dbProviderFactory.CreateConnection())
            {
                db.ConnectionString = _connectionString;
                await db.OpenAsync();

                for (int i = 0; i < count; i++)
                {
                    results[i] = await ReadSingleRow(db);
                }

                // postgres has problems with deadlocks when these aren't sorted
                Array.Sort<World>(results, (a, b) => a.Id.CompareTo(b.Id));

                for (int i = 0; i < count; i++)
                {
                    var randomNumber = _random.Next(1, 10001);
                    parameters[BatchUpdateString.Strings[i].Random] = randomNumber;
                    parameters[BatchUpdateString.Strings[i].Id] = results[i].Id;

                    results[i].RandomNumber = randomNumber;
                    updateCommand.Append(BatchUpdateString.Strings[i].UpdateQuery);
                }

                await db.ExecuteAsync(updateCommand.ToString(), parameters);
            }

            return results;
        }

        public async Task<IEnumerable<Fortune>> LoadFortunesRows()
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
}
