// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
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

        public int Next(int fromInclusive, int toExclusive)
        {
            // The total possible range is [0, 4,294,967,295).
            // Subtract one to account for zero being an actual possibility.
            uint range = (uint)toExclusive - (uint)fromInclusive - 1;

            // If there is only one possible choice, nothing random will actually happen, so return
            // the only possibility.
            if (range == 0)
            {
                return fromInclusive;
            }

            // Create a mask for the bits that we care about for the range. The other bits will be
            // masked away.
            uint mask = range;
            mask |= mask >> 1;
            mask |= mask >> 2;
            mask |= mask >> 4;
            mask |= mask >> 8;
            mask |= mask >> 16;

            Span<uint> resultSpan = stackalloc uint[1];
            uint result;

            do
            {
                Random.NextBytes(MemoryMarshal.AsBytes(resultSpan));
                result = mask & resultSpan[0];
            }
            while (result > range);

            return (int)result + fromInclusive;
        }
    }
}
