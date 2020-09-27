using System;

using GenHTTP.Modules.Webservices;

using Benchmarks.Model;
using System.Linq;

namespace Benchmarks.Tests
{

    public class DbResource
    {
        private static Random _Random = new Random();

        [Method]
        public World GetRandomWorld()
        {
            var id = _Random.Next(1, 10001);

            using var context = DatabaseContext.Create();

            return context.World.First(w => w.Id == id);
        }

    }

}
