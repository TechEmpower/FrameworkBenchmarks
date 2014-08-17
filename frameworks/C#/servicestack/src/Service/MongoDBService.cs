using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

using ServiceStack.Common;
using ServiceStack.ServiceHost;
using ServiceStack.ServiceInterface;

using ServiceStackBenchmark.Model;

using MongoDB.Bson;
using MongoDB.Driver.Builders;
using MongoDB.Driver;

namespace ServiceStackBenchmark
{

    #region MongoDB Service Requests

    [Api("Test #2 using Service Stack and MongoDB")]
    public class MongoDBDbRequest : IReturn<World>
    { }

    [Api("Test #3 using Service Stack and MongoDB")]
    public class MongoDBQueriesRequest : IReturn<List<World>>
    {
        [ApiMember(Name = "queries", Description = "Number of Queries to Execute", DataType = "int", IsRequired = true)]
        [ApiAllowableValues("queries", 1, 500)]
        public int queries { get; set; }
    }

    [Api("Test #4 using Service Stack, and MongoDB")]
    public class MongoDBFortunesRequest : IReturn<List<Fortune>>
    { }

    [Api("Test #5 using Service Stack, and MongoDB")]
    public class MongoDBUpdatesRequest : IReturn<List<World>>
    {
        [ApiMember(Name = "queries", Description = "Number of Queries to Execute", DataType = "int", IsRequired = true)]
        [ApiAllowableValues("queries", 1, 500)]
        public int queries { get; set; }
    }

    [Api("Test #7 using Service Stack, and MongoDB with Caching")]
    public class MongoDBCachedDbRequest : IReturn<World>
    { }

    #endregion

    /// <summary>Service Stack tests using MongoDB provider</summary>
    public class MongoDBService : Service
    {
        private const string dbType = "MongoDB";

        #region Public Properties

        public MongoDatabase db { get; set; }

        #endregion

        #region Public Service Methods

        public object Get(MongoDBDbRequest request)
        {
            // get a random world id
            var id = SafeRandom.Instance.Next(0, 10000) + 1;

            // retrieve world from database
            return db.GetWorld(id);
        }

        public object Get(MongoDBQueriesRequest request)
        {
            // limit queries to be between 1 and 500 iterations
            var worldCount = Math.Max(1, Math.Min(500, (int)request.queries));

            // concurrently create a list of random world ids to retrieve
            var ids = new List<int>();
            Parallel.For(0, worldCount, i =>
            {
                lock (ids)
                {
                    ids.Add(SafeRandom.Instance.Next(0, 10000) + 1);
                }
            });

            // retrieve worlds associated with ids
            return db.GetWorlds(ids);
        }

        [AddHeader(ContentType = ServiceStack.Common.Web.ContentType.Html)]
        public object Get(MongoDBFortunesRequest request)
        {
            // retrieve fortunes from database
            var fortunes = db.GetFortunes();

            // add additional fortune record
            fortunes.Add(new Fortune { id = 0, message = "Additional fortune added at request time." });

            // sort fortunes
            fortunes.Sort();

            // construct HTML page using template and return
            return FortuneMethods.ToHtml(fortunes);
        }

        public object Get(MongoDBUpdatesRequest request)
        {
            // limit queries to be between 1 and 500 iterations
            var worldCount = Math.Max(1, Math.Min(500, (int)request.queries));

            // concurrently create a list of random world ids to update
            var ids = new List<int>(worldCount);
            Parallel.For(0, worldCount, i =>
            {
                lock (ids)
                {
                    ids.Add(SafeRandom.Instance.Next(0, 10000) + 1);
                }
            });

            // purge cache client
            Cache.FlushAll();

            // update the worlds
            return db.UpdateWorlds(ids, 10000);
        }

        public object Get(MongoDBCachedDbRequest request)
        {
            // get a random world id
            var id = SafeRandom.Instance.Next(0, 10000) + 1;

            // create the cache key for the random world id
            var cacheKey = UrnId.CreateWithParts<World>(new string[] { dbType, id.ToString() });

            // if world is cached, return it
            var world = Cache.Get<World>(cacheKey);
            if (world != null)
                return world;

            // get all of the worlds form the database            
            var worlds = db.GetWorlds();

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