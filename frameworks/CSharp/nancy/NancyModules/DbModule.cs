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
        
        /**
         * NOTE:
         * Return a single World objects as JSON, selected randomly from the World
         * table.  Assume the table has 10,000 rows.
         */
        public DbModule() : base("/db")
        {
            Get["/{queries?1}"] = paramz =>
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
