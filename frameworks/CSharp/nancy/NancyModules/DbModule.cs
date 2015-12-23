namespace NancyModules
{
    using System;
    using System.Configuration;
    using System.Data;
    using System.Linq;
    using Dapper;
    using MySql.Data.MySqlClient;
    using Nancy;

    public class DbModule : NancyModule
    {
        public static string MYSQL_CONNECTION_STRING;
        
        static DbModule()
        {
            MYSQL_CONNECTION_STRING = ConfigurationManager.AppSettings["ConnectionString.MySQL"];
        }

        public DbModule() : base("/db")
        {
            //Console.WriteLine("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");

            Get["/{queries?1}"] = paramz =>
            {
                var queries = (int)paramz.queries;
                
                var random = new Random();
                using (var db = new MySqlConnection(MYSQL_CONNECTION_STRING))
                {
                    db.Open();

                    if (queries == 1)
                        return Response.AsJson(GetRandomWorld(db, random));
                    else
                    {
                        var worldCount = queries > 500 ? 500 : queries;
                        worldCount = worldCount < 1 ? 1 : worldCount;

                        // NOTE: Experiment with running the DB requests in parallel, on both Mono and Windows CLRs.
                        var worlds = new World[worldCount];

                        for (int i = 0; i < worldCount; ++i)
                        {
                            worlds[i] = GetRandomWorld(db, random);
                        }
                        //Console.WriteLine("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
                        return Response.AsJson(worlds);
                    }
                }
            };
            /*
             *  NOT SURE THIS IS CORRECT,but since this method covers test quries it must return JSON which ahs id and randomNumber
             */
            Get["foo"] = parmaz => 
            {
                var random = new Random();
                using (var db = new MySqlConnection(MYSQL_CONNECTION_STRING))
                {
                    db.Open();
                    return Response.AsJson(GetRandomWorld(db, random));
                }

            };
        }

        private World GetRandomWorld(IDbConnection db, Random random)
        {
            var id = random.Next(1, 10001);
            return db.Query<World>("SELECT id, randomNumber FROM world WHERE id = @id", new { id = id }).Single();
        }
    }

    public class World
    {
        public int id { get; set; }
        public int randomNumber { get; set; }
    }
}
