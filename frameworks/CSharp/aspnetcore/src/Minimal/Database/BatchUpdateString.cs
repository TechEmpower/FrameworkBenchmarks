// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System.Text;

namespace Minimal.Database;

internal class BatchUpdateString
{
    private const int MaxBatch = 500;

    private static readonly string[] _queries = new string[MaxBatch + 1];

    public static string Query(int batchSize)
    {
        if (_queries[batchSize] != null)
        {
            return _queries[batchSize];
        }

        var lastIndex = batchSize - 1;

        var sb = new StringBuilder();

        sb.Append("UPDATE world SET randomNumber = temp.randomNumber FROM (VALUES ");
        Enumerable.Range(0, lastIndex).ToList().ForEach(i => sb.Append($"(@Id_{i}, @Rn_{i}), "));
        sb.Append($"(@Id_{lastIndex}, @Rn_{lastIndex}) ORDER BY 1) AS temp(id, randomNumber) WHERE temp.id = world.id");

        return _queries[batchSize] = sb.ToString();
    }
}