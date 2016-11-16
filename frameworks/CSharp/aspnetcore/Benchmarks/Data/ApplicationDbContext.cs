// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System.Linq;
using Benchmarks.Configuration;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Infrastructure;
using Microsoft.EntityFrameworkCore.Storage.Internal;
using Microsoft.EntityFrameworkCore.Infrastructure.Internal;
using Microsoft.Extensions.Options;
using Microsoft.Extensions.DependencyInjection;

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

        public bool UseBatchUpdate 
        { 
            get
            {
                return _appSettings.Database != DatabaseServer.PostgreSql;
            }
        } 

        protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
        {
            if (_appSettings.Database == DatabaseServer.PostgreSql)
            {
                optionsBuilder.UseNpgsql(_appSettings.ConnectionString);
            }
            else
            {
                var extension = GetOrCreateExtension(optionsBuilder);
                extension.ConnectionString = _appSettings.ConnectionString;
                ((IDbContextOptionsBuilderInfrastructure)optionsBuilder).AddOrUpdateExtension(extension);
            }
        }

        private static SqlServerOptionsExtension GetOrCreateExtension(DbContextOptionsBuilder optionsBuilder)
        {
            var existing = optionsBuilder.Options.FindExtension<NoTxSqlServerOptionsExtension>();
            return existing != null
                ? new NoTxSqlServerOptionsExtension(existing)
                : new NoTxSqlServerOptionsExtension();
        }

        private class NoTxSqlServerOptionsExtension : SqlServerOptionsExtension
        {
            public NoTxSqlServerOptionsExtension()
            {
            }

            public NoTxSqlServerOptionsExtension(NoTxSqlServerOptionsExtension copyFrom) : base(copyFrom)
            {
            }
            public override void ApplyServices(IServiceCollection services)
            {
                base.ApplyServices(services);
                services.Remove(services.First((sd) => sd.ServiceType == typeof(ISqlServerConnection)));
                services.AddScoped<ISqlServerConnection, NoTransactionSqlServerConnection>();
            }
        }
    }
}