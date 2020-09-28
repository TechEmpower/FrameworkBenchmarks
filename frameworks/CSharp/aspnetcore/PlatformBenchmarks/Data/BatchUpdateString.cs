// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System.Linq;

namespace PlatformBenchmarks
{
    internal class BatchUpdateString
    {
        private const int MaxBatch = 500;

        public static DatabaseServer DatabaseServer;

        internal static readonly string[] Ids = Enumerable.Range(0, MaxBatch).Select(i => $"@I{i}").ToArray();
        internal static readonly string[] Randoms = Enumerable.Range(0, MaxBatch).Select(i => $"@R{i}").ToArray();

        private static string[] _queries = new string[MaxBatch + 1];

        public static string Query(int batchSize)
        {
            if (_queries[batchSize] != null)
            {
                return _queries[batchSize];
            }

            return CreateBatch(batchSize);
        }

        private static string CreateBatch(int batchSize)
        {
            var lastIndex = batchSize - 1;

            var sb = StringBuilderCache.Acquire();

            sb.AppendLine("UPDATE world SET randomNumber = CASE id");
            Enumerable.Range(0, batchSize).ToList().ForEach(i => sb.AppendLine($"when @I{i} then @R{i}"));
            sb.AppendLine("else randomnumber");
            sb.AppendLine("end");
            sb.Append("where id in (");
            Enumerable.Range(0, batchSize).ToList().ForEach(i => sb.AppendLine($"@I{i}{(lastIndex == i ? "" : ",")} "));
            sb.Append(")");

            return _queries[batchSize] = StringBuilderCache.GetStringAndRelease(sb);
        }
    }
}
