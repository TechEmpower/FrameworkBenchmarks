using System;
using System.Data.Entity;
using System.Data.Entity.ModelConfiguration.Configuration;
using System.Data.Entity.ModelConfiguration.Configuration.Properties.Primitive;
using System.Data.Entity.ModelConfiguration.Configuration.Types;
using System.Data.Entity.ModelConfiguration.Conventions;
using System.Reflection;

namespace Benchmarks.AspNet.Models
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
            // disable migrations checks
            Database.SetInitializer<EntityFramework>(null);

            modelBuilder.Conventions.Remove<PluralizingTableNameConvention>();

            modelBuilder.Ignore<MongoWorld>();
            
            if (Database.Connection is Npgsql.NpgsqlConnection)
                modelBuilder.Conventions.Add<PostgreSqlConfigurationConvention>();
        }
        
        private class PostgreSqlConfigurationConvention
            : IConfigurationConvention<Type, EntityTypeConfiguration>, 
              IConfigurationConvention<PropertyInfo, PrimitivePropertyConfiguration>,
              IConfigurationConvention<Type, ModelConfiguration>
        {
            public void Apply(Type memberInfo, Func<EntityTypeConfiguration> configuration)
            {
                configuration().ToTable(memberInfo.Name.ToLowerInvariant(), null);
            }
            
            public void Apply(PropertyInfo memberInfo, Func<PrimitivePropertyConfiguration> configuration)
            {
                configuration().ColumnName = memberInfo.Name.ToLowerInvariant();
            }
            
            public void Apply(Type memberInfo, Func<ModelConfiguration> configuration)
            {
                configuration().DefaultSchema = "public";
            }
        }
    }
}