﻿// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public class RawDb
    {
        private static readonly Comparison<World> WorldSortComparison = (a, b) => a.Id.CompareTo(b.Id);

        private readonly ConcurrentRandom _random;
        private readonly DbProviderFactory _dbProviderFactory;
        private readonly string _connectionString;

        public RawDb(ConcurrentRandom random, DbProviderFactory dbProviderFactory, AppSettings appSettings)
        {
            _random = random;
            _dbProviderFactory = dbProviderFactory;
            _connectionString = appSettings.ConnectionString;
        }

        public async Task<World> LoadSingleQueryRow()
        {
            using (var db = _dbProviderFactory.CreateConnection())
            {
                db.ConnectionString = _connectionString;
                await db.OpenAsync();

                using (var cmd = CreateReadCommand(db))
                {
                    return await ReadSingleRow(db, cmd);
                }
            }
        }

        async Task<World> ReadSingleRow(DbConnection connection, DbCommand cmd)
        {
            using (var rdr = await cmd.ExecuteReaderAsync(CommandBehavior.SingleRow))
            {
                await rdr.ReadAsync();

                return new World
                {
                    Id = rdr.GetInt32(0),
                    RandomNumber = rdr.GetInt32(1)
                };
            }
        }

        DbCommand CreateReadCommand(DbConnection connection)
        {
            var cmd = connection.CreateCommand();
            cmd.CommandText = "SELECT id, randomnumber FROM world WHERE id = @Id";
            var id = cmd.CreateParameter();
            id.ParameterName = "@Id";
            id.DbType = DbType.Int32;
            id.Value = _random.Next(1, 10001);
            cmd.Parameters.Add(id);

            (cmd as MySql.Data.MySqlClient.MySqlCommand)?.Prepare();

            return cmd;
        }


        public async Task<World[]> LoadMultipleQueriesRows(int count)
        {
            using (var db = _dbProviderFactory.CreateConnection())
            {
                db.ConnectionString = _connectionString;
                await db.OpenAsync();
                return await LoadMultipleRows(count, db);
            }

        }

        private async Task<World[]> LoadMultipleRows(int count, DbConnection db)
        {
            using (var cmd = CreateReadCommand(db))
            {
                cmd.Parameters["@Id"].Value = _random.Next(1, 10001);

                var result = new World[count];
                for (int i = 0; i < result.Length; i++)
                {
                    result[i] = await ReadSingleRow(db, cmd);
                    cmd.Parameters["@Id"].Value = _random.Next(1, 10001);
                }
                return result;
            }
        }

        public async Task<World[]> LoadMultipleUpdatesRows(int count)
        {
            using (var db = _dbProviderFactory.CreateConnection())
            {
                db.ConnectionString = _connectionString;
                await db.OpenAsync();

                var results = await LoadMultipleRows(count, db);

                // Postgres has problems with deadlocks when these aren't sorted
                Array.Sort<World>(results, WorldSortComparison);

                using (var updateCmd = db.CreateCommand())
                {
                    for (int i = 0; i < results.Length; i++)
                    {
                        var strings = BatchUpdateString.Strings[i];
                        var id = updateCmd.CreateParameter();
                        id.ParameterName = strings.Id;
                        id.DbType = DbType.Int32;
                        updateCmd.Parameters.Add(id);

                        var random = updateCmd.CreateParameter();
                        random.ParameterName = strings.Random;
                        random.DbType = DbType.Int32;
                        updateCmd.Parameters.Add(random);

                        var randomNumber = _random.Next(1, 10001);
                        id.Value = results[i].Id;
                        random.Value = randomNumber;
                        results[i].RandomNumber = randomNumber;
                    }

                    updateCmd.CommandText = BatchUpdateString.Strings[results.Length - 1].UpdateQuery;
                    await updateCmd.ExecuteNonQueryAsync();

                    return results;
                }
            }
        }

        public async Task<List<Fortune>> LoadFortunesRows()
        {
            var result = new List<Fortune>();

            using (var db = _dbProviderFactory.CreateConnection())
            using (var cmd = db.CreateCommand())
            {
                cmd.CommandText = "SELECT id, message FROM fortune";

                db.ConnectionString = _connectionString;
                await db.OpenAsync();

                (cmd as MySql.Data.MySqlClient.MySqlCommand)?.Prepare();

                using (var rdr = await cmd.ExecuteReaderAsync(CommandBehavior.CloseConnection))
                {
                    while (await rdr.ReadAsync())
                    {
                        result.Add(new Fortune
                        {
                            Id = rdr.GetInt32(0),
                            Message = rdr.GetString(1)
                        });
                    }
                }
            }

            result.Add(new Fortune { Message = "Additional fortune added at request time." });
            result.Sort();

            return result;
        }
    }
}
