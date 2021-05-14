// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using Benchmarks.Configuration;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Options;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Benchmarks.Data
{
    public class EfDb : IDb
    {
        private readonly IRandom _random;
        private readonly ApplicationDbContext _dbContext;

        public EfDb(IRandom random, ApplicationDbContext dbContext, IOptions<AppSettings> appSettings)
        {
            _random = random;
            _dbContext = dbContext;
        }

        private static readonly Func<ApplicationDbContext, int, Task<World>> _firstWorldQuery
            = EF.CompileAsyncQuery((ApplicationDbContext context, int id)
                => context.World.First(w => w.Id == id));

        public Task<World> LoadSingleQueryRow()
        {
            var id = _random.Next(1, 10001);

            return _firstWorldQuery(_dbContext, id);
        }

        public async Task<World[]> LoadMultipleQueriesRows(int count)
        {
            var result = new World[count];

            for (var i = 0; i < count; i++)
            {
                var id = _random.Next(1, 10001);

                result[i] = await _firstWorldQuery(_dbContext, id);
            }

            return result;
        }

        private static readonly Func<ApplicationDbContext, int, Task<World>> _firstWorldTrackedQuery
            = EF.CompileAsyncQuery((ApplicationDbContext context, int id)
                => context.World.AsTracking().First(w => w.Id == id));

        public async Task<World[]> LoadMultipleUpdatesRows(int count)
        {
            var results = new World[count];
            var usedIds = new HashSet<int>(count);
            
            for (var i = 0; i < count; i++)
            {
                int id;
                do
                {
                    id = _random.Next(1, 10001);
                } while (!usedIds.Add(id));
                
                results[i] = await _firstWorldTrackedQuery(_dbContext, id);

                results[i].RandomNumber = _random.Next(1, 10001);

                _dbContext.Entry(results[i]).State = EntityState.Modified;
            }

            await _dbContext.SaveChangesAsync();

            return results;
        }

        private static readonly Func<ApplicationDbContext, IAsyncEnumerable<Fortune>> _fortunesQuery
            = EF.CompileAsyncQuery((ApplicationDbContext context) => context.Fortune);

        public async Task<List<Fortune>> LoadFortunesRows()
        {
            var result = new List<Fortune>();

            await foreach (var element in _fortunesQuery(_dbContext))
            {
                result.Add(element);
            }

            result.Add(new Fortune { Message = "Additional fortune added at request time." });
            result.Sort();

            return result;
        }
    }
}
