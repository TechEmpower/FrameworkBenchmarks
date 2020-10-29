﻿using System;
using System.Collections.Generic;
using System.Linq;

using Benchmarks.Model;

using GenHTTP.Modules.Webservices;

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

            using (var context = DatabaseContext.Create())
            {
                var ids = Enumerable.Range(1, 10000).Select(x => _Random.Next(1, 10001)).Distinct().Take(count).ToArray();

                foreach (var id in ids)
                {
                    var record = context.World.First(w => w.Id == id);

                    var old = record.RandomNumber;

                    do
                    {
                        record.RandomNumber = _Random.Next(1, 10001);
                    }
                    while (old == record.RandomNumber);

                    result.Add(record);
                }

                context.SaveChanges();
            }


            return result;
        }

    }

}
