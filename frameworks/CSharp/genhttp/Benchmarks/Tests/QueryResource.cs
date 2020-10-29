using System;
using System.Collections.Generic;
using System.Linq;

using Benchmarks.Model;

using GenHTTP.Modules.Webservices;

namespace Benchmarks.Tests
{

    public class QueryResource
    {
        private static Random _Random = new Random();

        [ResourceMethod(":queries")]
        public List<World> GetWorldsFromPath(string queries) => GetWorlds(queries);

        [ResourceMethod]
        public List<World> GetWorlds(string queries)
        {
            var count = 1;

            int.TryParse(queries, out count);

            if (count < 1) count = 1;
            else if (count > 500) count = 500;

            var result = new List<World>(count);

            using var context = DatabaseContext.Create();

            for (int _ = 0; _ < count; _++)
            {
                var id = _Random.Next(1, 10001);

                result.Add(context.World.First(w => w.Id == id));
            }

            return result;
        }

    }

}
