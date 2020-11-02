using Benchmarks.Model;
using GenHTTP.Modules.Webservices;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Benchmarks.Tests
{

    public class UpdateResource
    {
        private static Random _Random = new Random();

        [ResourceMethod(":queries")]
        public List<World> UpdateWorldsFromPath(string queries) => UpdateWorlds(queries);

        [ResourceMethod]
        public List<World> UpdateWorlds(string queries)
        {
            var count = 1;

            int.TryParse(queries, out count);

            if (count < 1) count = 1;
            else if (count > 500) count = 500;

            var result = new List<World>(count);

            var ids = Enumerable.Range(1, 10000).Select(x => _Random.Next(1, 10001)).Distinct().Take(count).ToArray();

            foreach (var id in ids)
            {
                using (var context = DatabaseContext.Create())
                {
                    var record = context.World.First(w => w.Id == id);

                    var old = record.RandomNumber;

                    var current = old;

                    for (int i = 0; i < 5; i++)
                    {
                        current = _Random.Next(1, 10001);

                        if (current != old) break;
                    }

                    record.RandomNumber = current;

                    result.Add(record);

                    context.SaveChanges();
                }
            }

            return result;
        }

    }

}
