using System;
using System.Collections.Generic;
using System.Text;

namespace PlatformBenchmarks
{
    public class CachedWorld
    {
        public int Id { get; set; }

        public int RandomNumber { get; set; }

        public static implicit operator World(CachedWorld world) => new World { Id = world.Id, RandomNumber = world.RandomNumber };
        public static implicit operator CachedWorld(World world) => new CachedWorld { Id = world.Id, RandomNumber = world.RandomNumber };
    }
}
