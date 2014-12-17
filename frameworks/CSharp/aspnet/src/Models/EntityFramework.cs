using System.Data.Entity;
using System.Data.Entity.ModelConfiguration.Conventions;

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
                modelBuilder.Conventions.Add(new PostgreSqlConfigurationConvention());
        }
        
        private class PostgreSqlConfigurationConvention : Convention
        {
            public PostgreSqlConfigurationConvention()
            {
                Properties().Configure(p => p.HasColumnName(p.ClrPropertyInfo.Name.ToLowerInvariant()));
                Types().Configure(c => c.ToTable(c.ClrType.Name.ToLowerInvariant(), "public"));
            }
        }
    }
}