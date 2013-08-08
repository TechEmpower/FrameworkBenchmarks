using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Threading.Tasks;

using ServiceStack.Common;
using ServiceStack.ServiceHost;
using ServiceStack.ServiceInterface;

using ServiceStackBenchmark.Model;

namespace ServiceStackBenchmark
{

    #region SQLite Service Requests

    [Api("Test #2 using Service Stack, ORMLite, and SQLite")]
    public class SQLiteDbRequest : IReturn<World>
    { }

    [Api("Test #3 using Service Stack, ORMLite, and SQLite")]
    public class SQLiteQueriesRequest : IReturn<List<World>>
    {
        [ApiMember(Name = "queries", Description = "Number of Queries to Execute", DataType = "int", IsRequired = true)]
        [ApiAllowableValues("queries", 1, 500)]
        public int queries { get; set; }
    }

    [Api("Test #4 using Service Stack, ORMLite, and SQLite")]
    public class SQLiteFortunesRequest : IReturn<List<Fortune>>
    { }

    [Api("Test #5 using Service Stack, ORMLite, and SQLite")]
    public class SQLiteUpdatesRequest : IReturn<List<World>>
    {
        [ApiMember(Name = "queries", Description = "Number of Queries to Execute", DataType = "int", IsRequired = true)]
        [ApiAllowableValues("queries", 1, 500)]
        public int queries { get; set; }
    }

    [Api("Test #7 using Service Stack, ORMLite, and SQLite with Caching")]
    public class SQLiteCachedDbRequest : IReturn<World>
    { }

    #endregion

    /// <summary>Service Stack tests using SQLite provider and ORMLite</summary>
    public class SQLiteService : Service
    {
        private const string dbType = "SQLite";

        #region Public Properties

        public IDbConnection db { get; set; }

        #endregion

        #region Public Service Methods

        public object Get(SQLiteDbRequest request)
        {
            // get a random world id
            var id = SafeRandom.Instance.Next(1, 10000);

            // retrieve world from database
            return db.GetWorld(id);
        }

        public object Get(SQLiteQueriesRequest request)
        {
            // limit queries to be between 1 and 500 iterations
            var worldCount = Math.Max(1, Math.Min(500, (int)request.queries));

            // concurrently create a list of random world ids to retrieve
            var ids = new List<int>();
            Parallel.For(0, worldCount, i =>
            {
                lock (ids)
                {
                    ids.Add(SafeRandom.Instance.Next(1, 10000));
                }
            });

            // retrieve worlds associated with ids
            return db.GetWorlds(ids);
        }

        [AddHeader(ContentType = ServiceStack.Common.Web.ContentType.Html)]
        public object Get(SQLiteFortunesRequest request)
        {
            var fortunes = new List<Fortune>();

            // retrieve fortunes from database
            //using (var db = dbFactory.OpenDbConnection())
            //{
                fortunes = db.GetFortunes();
            //}

            // add additional fortune record
            fortunes.Add(new Fortune { id = 0, message = "Additional fortune added at request time." });

            // sort fortunes
            fortunes.Sort();

            // construct HTML page using template and return
            return FortuneMethods.ToHtml(fortunes);
        }

        public object Get(SQLiteUpdatesRequest request)
        {
            // limit queries to be between 1 and 500 iterations
            var worldCount = Math.Max(1, Math.Min(500, (int)request.queries));

            // concurrently create a list of random world ids to update
            var ids = new List<int>(worldCount);
            Parallel.For(0, worldCount, i =>
            {
                lock (ids)
                {
                    ids.Add(SafeRandom.Instance.Next(1, 10000));
                }
            });

            // purge cache client
            Cache.FlushAll();

            // update the worlds
            return db.UpdateWorlds(ids);
        }

        public object Get(SQLiteCachedDbRequest request)
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
            List<World> worlds = db.GetWorlds();

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