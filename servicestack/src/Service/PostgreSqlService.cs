using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

using ServiceStack.CacheAccess;
using ServiceStack.Common;
using ServiceStack.ServiceHost;
using ServiceStack.ServiceInterface;

using ServiceStackBenchmark.Model;

namespace ServiceStackBenchmark
{

    #region PostgreSQL Service Requests

    [Api("Test #2 using Service Stack, ORMLite, and PostgreSQL")]
    [Route("/postgresql/db", "GET")]
    public class PostgreSqlDbRequest : IReturn<World>
    { }

    [Api("Test #3 using Service Stack, ORMLite, and PostgreSQL")]
    [Route("/postgresql/queries/{queries}", "GET")]
    public class PostgreSqlQueriesRequest : IReturn<List<World>>
    {
        [ApiMember(Name = "queries", Description = "Number of Queries to Execute", DataType = "int", IsRequired = true)]
        [ApiAllowableValues("queries", 1, 500)]
        public int queries { get; set; }
    }

    [Api("Test #4 using Service Stack, ORMLite, and PostgreSQL")]
    [Route("/postgresql/fortunes", "GET")]
    public class PostgreSqlFortunesRequest : IReturn<List<Fortune>>
    { }

    [Api("Test #5 using Service Stack, ORMLite, and PostgreSQL")]
    [Route("/postgresql/updates/{queries}", "GET")]
    public class PostgreSqlUpdatesRequest : IReturn<List<World>>
    {
        [ApiMember(Name = "queries", Description = "Number of Queries to Execute", DataType = "int", IsRequired = true)]
        [ApiAllowableValues("queries", 1, 500)]
        public int queries { get; set; }
    }

    [Api("Test #7 using Service Stack, ORMLite, and PostgreSQL with Caching")]
    [Route("/postgresql/cached/db", "GET")]
    public class PostgreSqlCachedDbRequest : IReturn<World>
    { }

    #endregion

    /// <summary>Service Stack tests using PostgreSQL provider and ORMLite</summary>
    public class PostgreSqlService : Service
    {
        private const string dbType = "PgSql";

        #region Public Properties

        public IPostgreSqlOrmLiteConnectionFactory dbFactory { get; set; }

        #endregion

        #region Public Service Methods

        public object Get(PostgreSqlDbRequest request)
        {
            // get a random world id
            var id = SafeRandom.Instance.Next(1, 10000);

            // retrieve world from database
            using (var db = dbFactory.OpenDbConnection())
            {
                return db.GetWorld(id);
            }
        }

        public object Get(PostgreSqlQueriesRequest request)
        {
            // limit queries to be between 1 and 500 iterations
            var worldCount = Math.Max(1, Math.Min(500, (int)request.queries));

            // concurrently create a list of random world ids to retrieve
            var ids = new List<int>();
            Parallel.For(1, worldCount, i =>
            {
                lock (ids)
                {
                    ids.Add(SafeRandom.Instance.Next(1, 10000));
                }
            });

            // retrieve worlds associated with ids
            using (var db = dbFactory.OpenDbConnection())
            {
                return db.GetWorlds(ids);
            }
        }

        [AddHeader(ContentType = ServiceStack.Common.Web.ContentType.Html)]
        public object Get(PostgreSqlFortunesRequest request)
        {
            var fortunes = new List<Fortune>();

            // retrieve fortunes from database
            using (var db = dbFactory.OpenDbConnection())
            {
                fortunes = db.GetFortunes();
            }

            // add additional fortune record
            fortunes.Add(new Fortune { id = 0, message = "Additional fortune added at request time." });

            // sort fortunes
            fortunes.Sort();

            // construct HTML page using template and return
            return FortuneMethods.ToHtml(fortunes);
        }

        public object Get(PostgreSqlUpdatesRequest request)
        {
            // limit queries to be between 1 and 500 iterations
            var worldCount = Math.Max(1, Math.Min(500, (int)request.queries));

            // concurrently create a list of random world ids to update
            var ids = new List<int>(worldCount);
            Parallel.For(1, worldCount, i =>
            {
                lock (ids)
                {
                    ids.Add(SafeRandom.Instance.Next(1, 10000));
                }
            });

            // purge cache client
            Cache.FlushAll();

            // update the worlds
            using (var db = dbFactory.OpenDbConnection())
            {
                return db.UpdateWorlds(ids);
            }
        }

        public object Get(PostgreSqlCachedDbRequest request)
        {
            // get a random world id
            var id = SafeRandom.Instance.Next(1, 10000);

            // create the cache key for the random world id
            var cacheKey = UrnId.CreateWithParts<World>(new string[] { dbType, id.ToString() });

            // if world is cached, return it
            var world = Cache.Get<World>(cacheKey);
            if (world != null)
                return world;

            // get all of the worlds form the database
            List<World> worlds;
            using (var db = dbFactory.OpenDbConnection())
            {
                worlds = db.GetWorlds();
            }

            // construct a cache dictionary
            var cacheDict = new Dictionary<string, World>();
            Parallel.ForEach(worlds, w =>
            {
                // collect the current result
                if (w.id == id)
                    world = w;

                // add world to cache dictionary
                var key = UrnId.CreateWithParts<World>(new string[] { dbType, w.id.ToString() });
                lock (cacheDict)
                {
                    cacheDict.Add(key, w);
                }
            });

            // populate cache
            Cache.SetAll<World>(cacheDict);

            // return current request
            return world;
        }

        #endregion
    }

}