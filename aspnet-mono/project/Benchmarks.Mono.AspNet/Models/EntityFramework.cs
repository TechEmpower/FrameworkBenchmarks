using System;
using System.Data.Entity;
using System.Data.Entity.ModelConfiguration.Configuration.Types;
using System.Data.Entity.ModelConfiguration.Conventions;

namespace Benchmarks.Mono.AspNet.Models
{
    public class EntityFramework : DbContext
    {
        public DbSet<World> Worlds { get; set; }
        public DbSet<Fortune> Fortunes { get; set; }
        
        public EntityFramework(string providerName)
            : base(providerName)
        {
        }
        
        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            modelBuilder.Conventions.Remove<PluralizingTableNameConvention>();
            
            if (Database.Connection is Npgsql.NpgsqlConnection)
            {
                modelBuilder.HasDefaultSchema("public");
                modelBuilder.Conventions.Add<LowerCaseConfigurationConvention>();
            }
        }
        
        private class LowerCaseConfigurationConvention : IConfigurationConvention<Type, EntityTypeConfiguration>
        {
            public void Apply(Type memberInfo, Func<EntityTypeConfiguration> configuration)
            {
                configuration().ToTable(memberInfo.Name.ToLowerInvariant(), null);
            }
        }
    }
}