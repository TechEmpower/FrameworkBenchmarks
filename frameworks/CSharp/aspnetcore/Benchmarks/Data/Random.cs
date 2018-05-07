// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System;
using System.Threading;

namespace Benchmarks.Data
{
    public class DefaultRandom : IRandom
    {
        private static int nextSeed = 0;
        // Random isn't thread safe
        private static readonly ThreadLocal<Random> _random = new ThreadLocal<Random>(() => new Random(Interlocked.Increment(ref nextSeed)));

        public int Next(int minValue, int maxValue)
        {
            return _random.Value.Next(minValue, maxValue);
        }
    }
}
