namespace Nancy.Benchmark
{
    using System;
    using System.Data;
    using System.Threading.Tasks;
    using Dapper;
    using MySqlConnector;
    using Nancy;

    public class DbModule : NancyModule
    {
        private readonly string ConnectionString;

        /**
         * NOTE:
         * Return a single World objects as JSON, selected randomly from the World
         * table.  Assume the table has 10,000 rows.
         */
        public DbModule(IAppConfiguration appConfig) : base("/db")
        {
            ConnectionString = appConfig.ConnectionString;

            Get("/{queries?1}", async args =>
            {
              var random = new Random();
              using (var db = new MySqlConnection(ConnectionString))
              {
                db.Open();
                return Response.AsJson(await GetRandomWorld(db, random));
              }
            });
        }

        private Task<World> GetRandomWorld(IDbConnection db, Random random)
        {
            var id = random.Next(1, 10001);
            return db.QueryFirstOrDefaultAsync<World>("SELECT id, randomNumber FROM world WHERE id = @id", new { id });
        }
    }

    public class World
    {
        public int id { get; set; }
        public int randomNumber { get; set; }
    }
}
