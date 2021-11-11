// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Runtime.InteropServices;

namespace Benchmarks.Data
{
    public class CachedWorld
    {
        public int id { get; set; }

        public int randomNumber { get; set; }

        public static implicit operator World(CachedWorld world) => new World { id = world.id, randomNumber = world.randomNumber };
        public static implicit operator CachedWorld(World world) => new CachedWorld { id = world.id, randomNumber = world.randomNumber };
    }
}
