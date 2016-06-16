// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using Benchmarks.Configuration;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Options;

namespace Benchmarks.Data
{
    public class ApplicationDbContext : DbContext
    {
        private readonly AppSettings _appSettings;

        public ApplicationDbContext(IOptions<AppSettings> appSettings)
        {
            _appSettings = appSettings.Value;
        }

        public DbSet<World> World { get; set; }

        public DbSet<Fortune> Fortune { get; set; }

        protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
        {
            optionsBuilder.UseSqlServer(_appSettings.ConnectionString);
        }
    }
}
