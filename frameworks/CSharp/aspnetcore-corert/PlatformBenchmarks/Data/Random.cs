// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System;
using System.Runtime.CompilerServices;
using System.Threading;

namespace PlatformBenchmarks
{
    public class ConcurrentRandom
    {
        private static int nextSeed = 0;

        // Random isn't thread safe
        [ThreadStatic]
        private static Random _random;

        private static Random Random => _random ?? CreateRandom();

        [MethodImpl(MethodImplOptions.NoInlining)]
        private static Random CreateRandom()
        {
            _random = new Random(Interlocked.Increment(ref nextSeed));
            return _random;
        }

        public int Next(int minValue, int maxValue)
        {
            return Random.Next(minValue, maxValue);
        }
    }
}
