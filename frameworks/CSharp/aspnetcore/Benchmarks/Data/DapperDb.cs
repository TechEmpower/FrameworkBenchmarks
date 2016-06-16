// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System.Collections.Generic;
using System.Data.Common;
using System.Threading.Tasks;
using Benchmarks.Configuration;
using Dapper;
using Microsoft.Extensions.Options;

namespace Benchmarks.Data
{
    public class DapperDb
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
                return await db.QueryFirstOrDefaultAsync<World>(
                    "SELECT [Id], [RandomNumber] FROM [World] WHERE [Id] = @Id",
                    new { Id = _random.Next(1, 10001) });
            }
        }

        public async Task<World[]> LoadMultipleQueriesRows(int count)
        {
            var result = new World[count];

            using (var db = _dbProviderFactory.CreateConnection())
            {
                db.ConnectionString = _connectionString;
                await db.OpenAsync();

                for (int i = 0; i < count; i++)
                {
                    result[i] = await db.QueryFirstOrDefaultAsync<World>(
                        "SELECT [Id], [RandomNumber] FROM [World] WHERE [Id] = @Id",
                        new { Id = _random.Next(1, 10001) });
                }

                db.Close();
            }

            return result;
        }

        public async Task<IEnumerable<Fortune>> LoadFortunesRows()
        {
            List<Fortune> result;

            using (var db = _dbProviderFactory.CreateConnection())
            {
                db.ConnectionString = _connectionString;

                // Note: don't need to open connection if only doing one thing; let dapper do it
                result = (await db.QueryAsync<Fortune>("SELECT [Id], [Message] FROM [Fortune]")).AsList();
            }

            result.Add(new Fortune { Message = "Additional fortune added at request time." });
            result.Sort();

            return result;
        }
    }
}
