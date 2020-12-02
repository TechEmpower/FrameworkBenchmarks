using System;
using System.Linq;

using GenHTTP.Modules.Webservices;

using Benchmarks.Model;

namespace Benchmarks.Tests
{

    public sealed class DbResource
    {
        private static Random _Random = new Random();

        [ResourceMethod]
        public World GetRandomWorld()
        {
            var id = _Random.Next(1, 10001);

            using var context = DatabaseContext.CreateNoTracking();

            return context.World.First(w => w.Id == id);
        }

    }

}
