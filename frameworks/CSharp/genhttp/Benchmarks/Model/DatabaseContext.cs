using Microsoft.EntityFrameworkCore;

namespace Benchmarks.Model
{

    public sealed class DatabaseContext : DbContext
    {
        private static DbContextOptions<DatabaseContext> _Options;

        #region Factory

        public static DatabaseContext Create()
        {
            return new DatabaseContext(_Options ??= GetOptions());
        }

        private static DbContextOptions<DatabaseContext> GetOptions()
        {
            var optionsBuilder = new DbContextOptionsBuilder<DatabaseContext>();
            optionsBuilder.UseNpgsql("Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=64;NoResetOnClose=true;Enlist=false;Max Auto Prepare=3");

            return optionsBuilder.Options;
        }

        private DatabaseContext(DbContextOptions options) : base(options) { }

        #endregion

        #region Entities

        public DbSet<World> World { get; set; }

        public DbSet<Fortune> Fortune { get; set; }

        #endregion

    }

}
