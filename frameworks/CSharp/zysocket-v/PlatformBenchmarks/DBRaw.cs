using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Text;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public class RawDb
    {

        private readonly ConcurrentRandom _random;

        private readonly DbProviderFactory _dbProviderFactory;

        private readonly string _connectionString;

        public RawDb(ConcurrentRandom random, DbProviderFactory dbProviderFactory)
        {
            _random = random;
            _dbProviderFactory = dbProviderFactory;
            _connectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=256;NoResetOnClose=true;Enlist=false;Max Auto Prepare=3";
            //_connectionString = "Server=192.168.1.55;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=256;NoResetOnClose=true;Enlist=false;Max Auto Prepare=3";
            OnCreateCommand();
        }

        private void OnCreateCommand()
        {
            SingleCommand = new Npgsql.NpgsqlCommand();
            SingleCommand.CommandText = "SELECT id, randomnumber FROM world WHERE id = @Id";
            var id = SingleCommand.CreateParameter();
            id.ParameterName = "@Id";
            id.DbType = DbType.Int32;
            id.Value = _random.Next(1, 10001);
            SingleCommand.Parameters.Add(id);
            FortuneCommand = new Npgsql.NpgsqlCommand();
            FortuneCommand.CommandText = "SELECT id, message FROM fortune";
        }

        private DbCommand SingleCommand;

        private DbCommand FortuneCommand;

        public async Task<World> LoadSingleQueryRow()
        {
            using (var db = _dbProviderFactory.CreateConnection())
            {
                db.ConnectionString = _connectionString;
                await db.OpenAsync();
                SingleCommand.Connection = db;
                SingleCommand.Parameters[0].Value = _random.Next(1, 10001);
                return await ReadSingleRow(db, SingleCommand);

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
            SingleCommand.Connection = db;
            SingleCommand.Parameters[0].Value = _random.Next(1, 10001);
            var result = new World[count];
            for (int i = 0; i < result.Length; i++)
            {
                result[i] = await ReadSingleRow(db, SingleCommand);
                SingleCommand.Parameters[0].Value = _random.Next(1, 10001);
            }
            return result;

        }

        public async Task<List<Fortune>> LoadFortunesRows()
        {
            var result = new List<Fortune>();

            using (var db = _dbProviderFactory.CreateConnection())
            {
                db.ConnectionString = _connectionString;
                await db.OpenAsync();
                FortuneCommand.Connection = db;
                using (var rdr = await FortuneCommand.ExecuteReaderAsync(CommandBehavior.CloseConnection))
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
