using System;
using System.Threading.Tasks;

using GenHTTP.Modules.Webservices;

using Benchmarks.Model;

namespace Benchmarks.Tests
{

    public sealed class DbResource
    {
        private static Random _Random = new Random();

        [ResourceMethod]
        public async ValueTask<World> GetRandomWorld()
        {
            var id = _Random.Next(1, 10001);

            using var context = DatabaseContext.CreateNoTracking();

            return await context.World.FindAsync(id);
        }

    }

}
