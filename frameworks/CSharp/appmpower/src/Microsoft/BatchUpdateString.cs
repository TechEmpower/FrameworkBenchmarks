// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System.Linq;

namespace PlatformBenchmarks
{
   internal class BatchUpdateString
   {
      private const int MaxBatch = 500;

      internal static readonly string[] Ids = Enumerable.Range(0, MaxBatch).Select(i => $"i{i}").ToArray();
      internal static readonly string[] Randoms = Enumerable.Range(0, MaxBatch).Select(i => $"r{i}").ToArray();
      internal static readonly string[] Jds = Enumerable.Range(0, MaxBatch).Select(i => $"j{i}").ToArray();

      private static string[] _queries = new string[MaxBatch + 1];

      public static string Query(int batchSize)
      {
         if (_queries[batchSize] != null)
         {
            return _queries[batchSize];
         }

         var lastIndex = batchSize - 1;

         var sb = StringBuilderCache.Acquire();

         //if (DatabaseServer == DatabaseServer.PostgreSql)
         //{
         //sb.Append("UPDATE world SET randomNumber = temp.randomNumber FROM (VALUES ");
         //Enumerable.Range(0, lastIndex).ToList().ForEach(i => sb.Append($"(@Id_{i}, @Random_{i}), "));
         //sb.Append($"(@Id_{lastIndex}, @Random_{lastIndex}) ORDER BY 1) AS temp(id, randomNumber) WHERE temp.id = world.id");
         //}
         //else
         //{
         //   Enumerable.Range(0, batchSize).ToList().ForEach(i => sb.Append($"UPDATE world SET randomnumber = @Random_{i} WHERE id = @Id_{i};"));
         //}

         //sb.Append("UPDATE world SET randomNumber = CAST(temp.randomNumber AS INTEGER) FROM (VALUES ");
         //Enumerable.Range(0, lastIndex).ToList().ForEach(i => sb.Append("(?, ?), "));
         //sb.Append("(?, ?) ORDER BY 1) AS temp(id, randomNumber) WHERE CAST(temp.id AS INTEGER) = world.id");

         sb.Append("UPDATE world SET randomNumber = CASE id ");

         for (int i = 0; i < batchSize; i++)
         {
            sb.Append("WHEN ? THEN ? ");
         }

         //Enumerable.Range(0, batchSize).ToList().ForEach(i => sb.Append("WHEN ? THEN ? "));
         sb.Append("ELSE randomnumber END WHERE id IN (");

         for (int i = 0; i < lastIndex; i++)
         {
            sb.Append("?, ");
         }

         //Enumerable.Range(0, lastIndex).ToList().ForEach(i => sb.Append("?, "));
         sb.Append("?)");

         return _queries[batchSize] = StringBuilderCache.GetStringAndRelease(sb);
      }
   }
}