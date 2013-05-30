using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
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
                    var worldCount = request.queries > 500 ? 500 : request.queries;
                    worldCount = worldCount < 1 ? 1 : worldCount;

                    // NOTE: Experiment with running the DB requests in parallel, on both Mono and Windows CLRs.
                    var worlds = new World[worldCount];

                    for (int i = 0; i < worldCount; ++i)
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