using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Threading.Tasks;

using ServiceStack.CacheAccess;
using ServiceStack.Common;
using ServiceStack.DataAnnotations;
using ServiceStack.OrmLite;

using MongoDB.Bson;
using MongoDB.Driver.Builders;
using MongoDB.Driver;

namespace ServiceStackBenchmark.Model
{
    [Alias("World")]
    public class World
    {
        [PrimaryKey()]
        public int id { get; set; }
        public int randomNumber { get; set; }
    }

    public static class WorldMethods
    {

        public static World GetWorld(this IDbConnection db, int id)
        {
            // retrieve the World with passed id
            return db.GetById<World>(id);
        }

        public static World GetWorld(this MongoDatabase db, int id)
        {
            // retrieve the World with passed id
            var collection = db.GetCollection<World>("World");
            return collection.FindOne(Query<World>.EQ(w => w.id, id));
        }

        public static List<World> GetWorlds(this IDbConnection db)
        {
            // retrieve all Worlds
            return db.Select<World>();
        }

        public static List<World> GetWorlds(this MongoDatabase db)
        {
            // retrieve all Worlds
            var collection = db.GetCollection<World>("World");
            return collection.FindAll().ToList();
        }

        public static List<World> GetWorlds(this IDbConnection db, IEnumerable<int> ids)
        {
            // retrieve the Worlds included passed ids
            return db.GetByIds<World>(ids);
        }

        public static List<World> GetWorlds(this MongoDatabase db, IEnumerable<int> ids)
        {
            // retrieve the Worlds included passed ids
            var collection = db.GetCollection<World>("World");
            return collection.Find(Query<World>.In(w => w.id, ids)).ToList();
        }

        public static List<World> UpdateWorlds(this IDbConnection db, IEnumerable<int> ids)
        {
            // get the worlds for the passed ids
            var worlds = db.GetByIds<World>(ids);

            // concurrently update each world with a new random number
            Parallel.ForEach(worlds, w =>
            {
                lock (worlds)
                {
                    w.randomNumber = SafeRandom.Instance.Next(0, 10000) + 1;
                }
            });

            // update the database with the above changes
            db.UpdateAll<World>(worlds);

            // return updated collection
            return worlds;
        }

        public static List<World> UpdateWorlds(this MongoDatabase db, IEnumerable<int> ids, int recordCount)
        {
            var collection = db.GetCollection<World>("World");

            // get the worlds for the passed ids
            var worlds = collection.Find(Query<World>.In(w => w.id, ids)).ToList();

            // concurrently update each world with a new random number
            Parallel.ForEach(worlds, w =>
            {
                lock (worlds)
                {
                    w.randomNumber = SafeRandom.Instance.Next(0, recordCount) + 1;
                }
            });

            // TODO: look into how to make this a single statement
            foreach (var w in worlds)
            {
                // update the database with the above changes
                collection.Update(
                    Query<World>.EQ(t => t.id, w.id),
                    Update<World>.Set(t => t.randomNumber, w.randomNumber));
            }

            // return updated collection
            return worlds;
        }


        public static void CacheAllWorlds(this IDbConnection db, ICacheClient cache, string dbType)
        {
            cache.FlushAll();

            // concurrently create a list of world ids
            var worlds = db.GetWorlds();

            Parallel.ForEach<World>(worlds, w =>
            {
                var cacheKey = UrnId.CreateWithParts<World>(new string[] { dbType, w.id.ToString() });

                cache.Set<World>(cacheKey, w);
            });
        }

        public static void CacheAllWorlds(this MongoDatabase db, ICacheClient cache, string dbType)
        {
            cache.FlushAll();

            // concurrently create a list of world ids
            var worlds = db.GetWorlds();

            Parallel.ForEach<World>(worlds, w =>
            {
                var cacheKey = UrnId.CreateWithParts<World>(new string[] { dbType, w.id.ToString() });

                cache.Set<World>(cacheKey, w);
            });
        }

        public static bool CreateWorldTable(this IDbConnection db)
        {
            // only create table if it does not already exist
            if (db.TableExists("World"))
                return true;

            try
            {
                // create the database table based on model
                db.CreateTable<World>();

                // populate the table
                var worlds = new List<World>(10000);
                Parallel.For(0, 10000, i =>
                    {
                        lock (worlds)
                        {
                            worlds.Add(new World() { id = i, randomNumber = SafeRandom.Instance.Next(0, 10000) + 1 });
                        }

                    });

                // insert new records into database
                db.Insert<World>(worlds.ToArray());

                return true;
            }
            catch
            {
                return false;
            }
        }

        public static bool CreateWorldTable(this MongoDatabase db)
        {
            // only create table if it does not already exist
            if (db.CollectionExists("World"))
                return true;

            try
            {
                // populate the table
                var worlds = new List<World>(10000);
                Parallel.For(0, 10000, i =>
                {
                    lock (worlds)
                    {
                        worlds.Add(new World() { id = i, randomNumber = SafeRandom.Instance.Next(0, 10000) + 1 });
                    }

                });

                // insert new records into database
                var collection = db.GetCollection<World>("World");
                collection.InsertBatch(worlds);

                return true;
            }
            catch
            {
                return false;
            }
        }

    }
}