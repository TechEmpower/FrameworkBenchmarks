using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Threading.Tasks;

using ServiceStack.CacheAccess;
using ServiceStack.Common;
using ServiceStack.DataAnnotations;
using ServiceStack.OrmLite;

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

        public static List<World> GetWorlds(this IDbConnection db)
        {
            // retrieve all Worlds
            return db.Select<World>();
        }

        public static List<World> GetWorlds(this IDbConnection db, IEnumerable<int> ids)
        {
            // retrieve the Worlds included passed ids
            return db.GetByIds<World>(ids);
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

            // update the dataase with the above changes
            db.UpdateAll<World>(worlds);

            // return updated collection
            return worlds;
        }

        public static void CacheAllWorlds(this IDbConnection db, ICacheClient cache)
        {
            cache.FlushAll();

            // concurrently create a list of world ids
            var worlds = db.GetWorlds();

            Parallel.ForEach<World>(worlds, w =>
            {
                var cacheKey = UrnId.Create<World>("Id", w.id.ToString());

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

    }
}