// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

namespace PlatformBenchmarks;

public sealed class CachedWorld
{
    public int Id { get; set; }

    public int RandomNumber { get; set; }

    public static implicit operator CachedWorld(World world) => new() { Id = world.Id, RandomNumber = world.RandomNumber };
}
