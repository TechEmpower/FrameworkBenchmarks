using System.Data.Entity;

namespace Benchmarks.Mono.AspNet.Models
{
    public class PostgreSqlWorldContext : DbContext
    {
        public DbSet<World> World { get; set; }

        public PostgreSqlWorldContext()
            : base("PostgreSQL")
        {
        }

        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            modelBuilder.Entity<World>()
                .HasKey(w => w.id)
                .Property(w => w.randomNumber).HasColumnName("randomnumber");

            modelBuilder.Entity<World>().ToTable("world", "public");
        }
    }
}