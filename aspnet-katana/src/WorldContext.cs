using System.Data.Entity;
using System.Data.Entity.ModelConfiguration.Conventions;

namespace Benchmarks.Katana
{
    public class WorldContext : DbContext
    {
        public WorldContext(string connection) : base(connection){}
   
        public DbSet<World> World { get; set; }

        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            // disable migrations check
            Database.SetInitializer<WorldContext>(null);
            // remove default convention to look for tables as the plural of the class name
            modelBuilder.Conventions.Remove<PluralizingTableNameConvention>();
            base.OnModelCreating(modelBuilder);
        }
    }
}