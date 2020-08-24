using System;
using System.Collections.Generic;
using System.Data;
using System.Runtime.CompilerServices;
// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Threading.Tasks;
using Npgsql;

namespace Benchmarks.Data
{
    public interface IRawDb    
    {
        Task<World> LoadSingleQueryRow();

        Task<List<Fortune>> LoadFortunesRows();
    }


    public class RawDb : IRawDb
    {
        private readonly string _connectionString;
        
        public RawDb(AppSettings appSettings)
        {
            _connectionString = appSettings.ConnectionString;
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

                db.ConnectionString = _connectionString;
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
    }
}