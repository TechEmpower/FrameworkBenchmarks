using System.Data.Entity;

namespace Benchmarks.Mono.AspNet.Models
{
    public class MySqlWorldContext : DbContext
    {
        public DbSet<World> World { get; set; }

        public MySqlWorldContext()
            : base("MySQL")
        {
        }

        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            modelBuilder.Entity<World>()
                .HasKey(w => w.id)
                .Property(w => w.randomNumber).HasColumnName("randomNumber");

            modelBuilder.Entity<World>().ToTable("world");
        }
    }
}