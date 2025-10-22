// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using appMpower.Orm; 
using appMpower.Orm.Data; 

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

         /*
         sb.Append("UPDATE world SET randomNumber = temp.randomNumber FROM (VALUES ");
         Enumerable.Range(0, lastIndex).ToList().ForEach(i => sb.Append("(?::int,?::int),"));
         sb.Append("(?::int,?::int) ORDER BY 1) AS temp(id, randomNumber) WHERE temp.id = world.id");
         //sb.Append("(?::int,?::int)) AS temp(id, randomNumber) WHERE temp.id = world.id");
         */

         if (Constants.Dbms == Dbms.MySQL)
         {
            for (int i = 0; i < batchSize; i++)
            {
               sb.Append("UPDATE world SET randomNumber=? WHERE id=?;");
            }
         }
         else
         {
            sb.Append("UPDATE world SET randomNumber=CASE id ");

            for (int i = 0; i < batchSize; i++)
            {
               sb.Append("WHEN ? THEN ? ");
            }

            sb.Append("ELSE randomnumber END WHERE id IN(");

            for (int i = 0; i < lastIndex; i++)
            {
               sb.Append("?,");
            }

            sb.Append("?)");
         }

         return _queries[batchSize] = StringBuilderCache.GetStringAndRelease(sb);
      }
   }
}