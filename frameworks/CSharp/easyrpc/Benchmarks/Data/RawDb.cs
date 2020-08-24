using System;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;
using Npgsql;

namespace Benchmarks.Data
{
    public interface IRawDb    
    {
        Task<World> LoadSingleQueryRow();
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