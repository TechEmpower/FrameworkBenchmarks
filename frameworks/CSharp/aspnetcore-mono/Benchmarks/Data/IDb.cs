// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System.Collections.Generic;
using System.Threading.Tasks;

namespace Benchmarks.Data
{
    public interface IDb
    {
        Task<World> LoadSingleQueryRow();

        Task<World[]> LoadMultipleQueriesRows(int count);

        Task<World[]> LoadMultipleUpdatesRows(int count);

        Task<IEnumerable<Fortune>> LoadFortunesRows();
    }
}
