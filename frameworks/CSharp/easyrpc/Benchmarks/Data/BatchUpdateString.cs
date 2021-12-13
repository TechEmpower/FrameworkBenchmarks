// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;

namespace Benchmarks.Data
{
    internal class BatchUpdateString
    {
        private const int MaxBatch = 500;

        internal static readonly string[] Ids = Enumerable.Range(0, MaxBatch).Select(i => $"@Id_{i}").ToArray();
        internal static readonly string[] Randoms = Enumerable.Range(0, MaxBatch).Select(i => $"@Random_{i}").ToArray();

        private static string[] _queries = new string[MaxBatch + 1];

        public static string Query(int batchSize)
        {
            if (_queries[batchSize] != null)
            {
                return _queries[batchSize];
            }

            var lastIndex = batchSize - 1;

            var sb = StringBuilderCache.Acquire();

            sb.Append("UPDATE world SET randomNumber = temp.randomNumber FROM (VALUES ");
            Enumerable.Range(0, lastIndex).ToList().ForEach(i => sb.Append($"(@Id_{i}, @Random_{i}), "));
            sb.Append($"(@Id_{lastIndex}, @Random_{lastIndex}) ORDER BY 1) AS temp(id, randomNumber) WHERE temp.id = world.id");

            return _queries[batchSize] = StringBuilderCache.GetStringAndRelease(sb);
        }
    }
}