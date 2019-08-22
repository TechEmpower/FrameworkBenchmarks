using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Text;
using System.Threading.Tasks;
using System.Collections.Concurrent;
namespace Benchmarks
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
            //_connectionString = "Server=192.168.2.19;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=256;NoResetOnClose=true;Enlist=false;Max Auto Prepare=3";


            for (int i = 0; i < 256; i++)
            {
                DbConnection conn = dbProviderFactory.CreateConnection();
                conn.ConnectionString = _connectionString;
                RawDbConnection rawDbConnection = new RawDbConnection(conn, this);
                mPool.Push(rawDbConnection);
            }
        }


        private ConcurrentStack<RawDbConnection> mPool = new ConcurrentStack<RawDbConnection>();


        private RawDbConnection Pop()
        {
            if (mPool.TryPop(out RawDbConnection conn))
                return conn;
            else
                throw new Exception("get raw db connection error!");
        }

        private void Push(RawDbConnection conn)
        {
            mPool.Push(conn);
        }


        class RawDbConnection : IDisposable
        {
            public RawDbConnection(DbConnection connection, RawDb rawdb)
            {
                Connection = connection;
                Connection.Open();

                var cmd = connection.CreateCommand();
                cmd.CommandText = "SELECT id, randomnumber FROM world WHERE id = @Id";
                var id = cmd.CreateParameter();
                id.ParameterName = "@Id";
                id.DbType = DbType.Int32;
                id.Value = 0;
                cmd.Parameters.Add(id);
                ReadCommand = cmd;

                cmd = connection.CreateCommand();
                cmd.CommandText = "SELECT id, message FROM fortune";
                FortuneCommand = cmd;

                DbHandler = rawdb;

            }

            public DbConnection Connection { get; private set; }

            public DbCommand ReadCommand { get; private set; }

            public DbCommand FortuneCommand { get; private set; }

            public RawDb DbHandler { get; private set; }

            public void Dispose()
            {
                DbHandler.Push(this);
            }
        }

        public async Task<World> LoadSingleQueryRow()
        {
            using (var conn = Pop())
            {
                var cmd = conn.ReadCommand;
                cmd.Parameters[0].Value = _random.Next(1, 10001);
                return await ReadSingleRow(conn.Connection, cmd);
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
            using (var conn = Pop())
            {
                var cmd = conn.ReadCommand;
                cmd.Parameters[0].Value = _random.Next(1, 10001);
                return await LoadMultipleRows(count, conn.Connection, conn.ReadCommand);
            }
        }

        private async Task<World[]> LoadMultipleRows(int count, DbConnection db, DbCommand cmd)
        {
            cmd.Parameters[0].Value = _random.Next(1, 10001);
            var result = new World[count];
            for (int i = 0; i < result.Length; i++)
            {
                result[i] = await ReadSingleRow(db, cmd);
                cmd.Parameters[0].Value = _random.Next(1, 10001);
            }
            return result;
        }

        public async Task<List<Fortune>> LoadFortunesRows()
        {
            var result = new List<Fortune>();
            using (var conn = Pop())
            {
                var cmd = conn.FortuneCommand;
                using (var rdr = await cmd.ExecuteReaderAsync(CommandBehavior.Default))
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
