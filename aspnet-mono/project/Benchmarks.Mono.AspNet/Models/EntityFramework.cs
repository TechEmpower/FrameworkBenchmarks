using System.Data.Entity;

namespace Benchmarks.Mono.AspNet.Models
{
    public class EntityFramework : DbContext
    {
        public DbSet<World> Worlds { get; set; }
        public DbSet<Fortune> Fortunes { get; set; }

        public EntityFramework()
            : base("MySQL")
        {
        }

        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            modelBuilder.Entity<World>()
                .HasKey(w => w.id)
                .Property(w => w.randomNumber).HasColumnName("randomNumber");

            modelBuilder.Entity<Fortune>()
                .HasKey(w => w.ID)
                .Property(w => w.Message);

            modelBuilder.Entity<World>().ToTable("World");
            modelBuilder.Entity<Fortune>().ToTable("Fortune");
        }
    }
}