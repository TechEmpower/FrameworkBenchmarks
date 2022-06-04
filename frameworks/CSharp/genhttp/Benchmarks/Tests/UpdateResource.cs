using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

using Microsoft.EntityFrameworkCore;

using Benchmarks.Model;

using GenHTTP.Modules.Webservices;

namespace Benchmarks.Tests
{

    public sealed class UpdateResource
    {
        private static Random _Random = new Random();

        [ResourceMethod(":queries")]
        public ValueTask<List<World>> UpdateWorldsFromPath(string queries) => UpdateWorlds(queries);

        [ResourceMethod]
        public async ValueTask<List<World>> UpdateWorlds(string queries)
        {
            var count = 1;

            int.TryParse(queries, out count);

            if (count < 1) count = 1;
            else if (count > 500) count = 500;

            var result = new List<World>(count);

            var ids = Enumerable.Range(1, 10000).Select(x => _Random.Next(1, 10001)).Distinct().Take(count).ToArray();

            using (var context = DatabaseContext.Create())
            {
                foreach (var id in ids)
                {
                    var record = await context.World.FirstOrDefaultAsync(w => w.Id == id).ConfigureAwait(false);

                    var old = record.RandomNumber;

                    var current = old;

                    for (int i = 0; i < 5; i++)
                    {
                        current = _Random.Next(1, 10001);

                        if (current != old) break;
                    }

                    record.RandomNumber = current;

                    result.Add(record);

                    await context.SaveChangesAsync();
                }
            }

            return result;
        }

    }

}
