namespace NancyModules
{
    using System;
    using System.Configuration;
    using System.Data;
    using System.Linq;
    using System.Threading.Tasks;
    using Dapper;
    using MySql.Data.MySqlClient;
    using Nancy;

    public class DbModule : NancyModule
    {
        public static string MysqlConnectionString = ConfigurationManager.AppSettings["ConnectionString.MySQL"];

        private static readonly Random Random = new Random();

        public DbModule()
        {
            Get["/db", runAsync: true] = (args, ct) => UsingConnection(GetRandomWorld);

            Get["/queries/{queries}", runAsync: true] = (args, ct) =>
            {
                int queries = args.queries.TryParse<int>(defaultValue: 1);

                return UsingConnection(db => Task.WhenAll(
                    Enumerable.Range(0, queries.Clamp(1, 500))
                        .Select(_ => GetRandomWorld(db))));
            };
        }

        private static async Task<World> GetRandomWorld(IDbConnection db)
        {
            var id = Random.Next(1, 10001);

            var result = await db.QueryAsync<World>("SELECT id, randomNumber FROM world WHERE id = @id", new { id });

            return result.Single();
        }

        private async Task<dynamic> UsingConnection<T>(Func<IDbConnection, Task<T>> function)
        {
            using (var db = new MySqlConnection(MysqlConnectionString))
            {
                await db.OpenAsync();

                var result = await function.Invoke(db);

                return Response.AsJson(result);
            }
        }

        private class World
        {
            public int id { get; set; }

            public int randomNumber { get; set; }
        }
    }
}