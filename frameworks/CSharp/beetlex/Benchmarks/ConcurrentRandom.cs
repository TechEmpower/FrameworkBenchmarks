using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading;

namespace Benchmarks
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
