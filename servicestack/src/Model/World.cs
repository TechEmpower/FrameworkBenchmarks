using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;

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

        public static World GetRandomWorld(this IDbConnection db, Random randomizer)
        {
            int id = randomizer.Next(1, 10000);
            return db.GetById<World>(id);
        }

        public static List<World> GetRandomWorlds(this IDbConnection db, int count, Random randomizer)
        {
            var worlds = new List<World>(count);

            for (int i = 0; i < count; ++i)
            {
                worlds.Add(GetRandomWorld(db, randomizer));
            }

            return worlds;
        }

        public static World UpdateRandomWorld(this IDbConnection db, Random randomizer)
        {
            var world = db.GetRandomWorld(randomizer);
            world.randomNumber = randomizer.Next(1, 10000);
            db.Update<World>(world);
            return world;
        }

        public static List<World> UpdateRandomWorlds(this IDbConnection db, int count, Random randomizer)
        {
            var worlds = new List<World>(count);

            for (int i = 0; i < count; ++i)
            {
                worlds.Add(UpdateRandomWorld(db, randomizer));
            }
            return worlds;
        }

        public static void CreateWorldTable(this IDbConnection db)
        {
            if (!db.TableExists("World"))
            {
                db.CreateTable<World>();

                // Populate the table
                var randomizer = new Random();
                var worlds = new List<World>();
                for (int i = 1; i < 10000; i++)
                {
                    worlds.Add(new World() { id = i, randomNumber = randomizer.Next(1, 10000) });
                }
                db.Insert<World>(worlds.ToArray());
            }
        }

    }
}