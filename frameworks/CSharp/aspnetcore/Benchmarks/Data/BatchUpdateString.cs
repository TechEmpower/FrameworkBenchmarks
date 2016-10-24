// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System.Collections.Generic;
using System.Linq;

namespace Benchmarks.Data
{
    internal class BatchUpdateString
    {
        public static IList<BatchUpdateString> Strings { get;} = 
            Enumerable.Range(0, 500)
                      .Select(i => new BatchUpdateString
                      {
                          Id = $"Id_{i}",
                          Random = $"Random_{i}",
                          UpdateQuery = $"UPDATE world SET randomnumber = @Random_{i} WHERE id = @Id_{i};"
                      }).ToArray();
                        
        public string Id { get; set; }
        public string Random { get; set; }
        public string UpdateQuery { get; set; }
    }
}