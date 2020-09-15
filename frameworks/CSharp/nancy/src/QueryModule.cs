namespace Nancy.Benchmark
{
    using System;
    using System.Data;
    using System.Threading.Tasks;
    using Dapper;
    using MySqlConnector;
    using Nancy;

    public class QueryModule : NancyModule
    {
        private readonly string ConnectionString;
        
        /**
         * Query:
         * Return a list of World objects as JSON, selected randomly from the World
         * table.  Assume the table has 10,000 rows.
         */
        public QueryModule(IAppConfiguration appConfig) : base("/queries")
        {
            ConnectionString = appConfig.ConnectionString;

            Get("/{queries?1}", async args =>
            {
                if (!int.TryParse((string)args.queries, out var queries) || queries < 1)
                {
                    queries = 1;
                }
                else if (queries > 500)
                {
                    queries = 500;
                }
                
                var random = new Random();
                using (var db = new MySqlConnection(ConnectionString))
                {
                    db.Open();

                    // NOTE: Experiment with running the DB requests in parallel, on both Mono and Windows CLRs.
                    var worlds = new World[queries];

                    for (int i = 0; i < worlds.Length; ++i)
                    {
                        worlds[i] = await GetRandomWorld(db, random);
                    }
                    return Response.AsJson( worlds );
                }
            });

            Get("/{name}", async args =>
            {
                if (!int.TryParse((string)args.name, out var queries) || queries < 1)
                {
                    queries = 1;
                }
                else if (queries > 500)
                {
                    queries = 500;
                }

                var random = new Random();
                using (var db = new MySqlConnection(ConnectionString))
                {
                    db.Open();

                    // NOTE: Experiment with running the DB requests in parallel, on both Mono and Windows CLRs.
                    var worlds = new World[queries];

                    for (int i = 0; i < worlds.Length; ++i)
                    {
                        worlds[i] = await GetRandomWorld(db, random);
                    }
                    return Response.AsJson( worlds );
                }

            });
        }

        private Task<World> GetRandomWorld(IDbConnection db, Random random)
        {
            var id = random.Next(1, 10001);
            return db.QueryFirstOrDefaultAsync<World>("SELECT id, randomNumber FROM world WHERE id = @id", new { id });
        }
    }

}