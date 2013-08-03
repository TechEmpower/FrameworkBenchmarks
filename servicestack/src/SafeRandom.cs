using System;
using System.Collections.Generic;
using System.Linq;

namespace ServiceStackBenchmark
{
    internal sealed class SafeRandom
    {
        private static volatile SafeRandom instance;

        private static object syncRoot = new object();

        private static Random random;

        private SafeRandom()
        {
            random = new Random();
        }

        public int Next()
        {
            int result;

            lock (random)
            {
                result = random.Next();
            }
            return result;
        }

        public int Next(int maxValue)
        {
            int result;

            lock (random)
            {
                result = random.Next(maxValue);
            }
            return result;
        }

        public int Next(int minValue, int maxValue)
        {
            int result;

            lock (random)
            {
                result = random.Next(minValue, maxValue);
            }
            return result;
        }

        public static SafeRandom Instance
        {
            get
            {
                if (instance == null)
                {
                    lock (syncRoot)
                    {
                        if (instance == null)
                        {
                            instance = new SafeRandom();
                        }
                    }
                }

                return instance;
            }
        }
    }
}