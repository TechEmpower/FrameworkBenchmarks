// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using Benchmarks.Configuration;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;

namespace Benchmarks.Data
{
    internal class BatchUpdateString
    {
        private const int MaxBatch = 500;
        private static string[] _queries = new string[MaxBatch];
        private static DatabaseServer _databaseServer;

        public static IList<BatchUpdateString> Strings { get; } =
            Enumerable.Range(0, MaxBatch)
                      .Select(i => new BatchUpdateString
                      {
                          Id = $"Id_{i}",
                          Random = $"Random_{i}",
                          BatchSize = i
                      }).ToArray();

        private int BatchSize { get; set; }
        public string Id { get; set; }
        public string Random { get; set; }
        public string UpdateQuery => _queries[BatchSize] ?? CreateQuery(BatchSize);

        [MethodImpl(MethodImplOptions.NoInlining)]
        private string CreateQuery(int batchSize)
        {
            var sb = StringBuilderCache.Acquire();
            if (_databaseServer == DatabaseServer.PostgreSql)
            {
                sb.Append("UPDATE world SET randomNumber = temp.randomNumber FROM (VALUES ");
                Enumerable.Range(0, batchSize).Select(i => sb.Append($"(@Id_{i}, @Random_{i}), ")).ToList();
                sb.Append($"(@Id_{batchSize}, @Random_{batchSize}) ORDER BY 1) AS temp(id, randomNumber) WHERE temp.id = world.id");
            }
            else
            {
                foreach (var q in Enumerable.Range(0, batchSize + 1)
                    .Select(i => $"UPDATE world SET randomnumber = @Random_{i} WHERE id = @Id_{i};"))
                {
                    sb.Append(q);
                }
            }
            var query = sb.ToString();
            _queries[batchSize] = query;
            return query;
        }

        public static void Initialize(DatabaseServer databaseServer)
        {
            _databaseServer = databaseServer;
            Observe(Strings[0].UpdateQuery);
            Observe(Strings[4].UpdateQuery);
            Observe(Strings[9].UpdateQuery);
            Observe(Strings[14].UpdateQuery);
            Observe(Strings[19].UpdateQuery);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        private static void Observe(string query)
        {
        }
    }
}