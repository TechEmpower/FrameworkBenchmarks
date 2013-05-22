using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Linq;
using System.Web;
using ServiceStack.OrmLite;
using ServiceStack.OrmLite.MySql;
using ServiceStack.ServiceHost;
using ServiceStack.ServiceInterface;

namespace ServiceStackBenchmark
{
    [Route("/db")]
    public class DbRequest
    {
        public int queries { get; set; }
    }

    public class World
    {
        public int id { get; set; }
        public int randomNumber { get; set; }
    }

    public class DbService : Service
    {
        private static readonly string MYSQL_CONNECTION_STRING;
        
        static DbService()
        {
            MYSQL_CONNECTION_STRING = ConfigurationManager.AppSettings["ConnectionString.MySQL"];
        }
        
        public object Get(DbRequest request)
        {
            OrmLiteConfig.DialectProvider = MySqlDialectProvider.Instance;

            var random = new Random();
            using (var db = MYSQL_CONNECTION_STRING.OpenDbConnection())
            {
                if (request.queries == 0)
                    return GetRandomWorld(db, random);
                else
                {
                    var worlds = new World[request.queries];

                    // TODO: Execute these concurrently (or is that cheating?)
                    for (int i = 0; i < request.queries; ++i)
                    {
                        worlds[i] = GetRandomWorld(db, random);
                    }
                    return worlds;
                }
            }
        }

        private World GetRandomWorld(IDbConnection db, Random random)
        {
            var id = random.Next(1, 10001);
            return db.GetById<World>(id);
        }
    }
}