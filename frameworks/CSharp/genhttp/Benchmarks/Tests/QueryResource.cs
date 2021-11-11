using System;
using System.Collections.Generic;
using System.Threading.Tasks;

using Microsoft.EntityFrameworkCore;

using Benchmarks.Model;

using GenHTTP.Modules.Webservices;

namespace Benchmarks.Tests
{

    public sealed class QueryResource
    {
        private static Random _Random = new Random();

        [ResourceMethod(":queries")]
        public ValueTask<List<World>> GetWorldsFromPath(string queries) => GetWorlds(queries);

        [ResourceMethod]
        public async ValueTask<List<World>> GetWorlds(string queries)
        {
            var count = 1;

            int.TryParse(queries, out count);

            if (count < 1) count = 1;
            else if (count > 500) count = 500;

            var result = new List<World>(count);

            using var context = DatabaseContext.CreateNoTracking();

            for (int _ = 0; _ < count; _++)
            {
                var id = _Random.Next(1, 10001);

                result.Add(await context.World.FirstOrDefaultAsync(w => w.Id == id).ConfigureAwait(false));
            }

            return result;
        }

    }

}
