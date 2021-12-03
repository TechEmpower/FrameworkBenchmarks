using Microsoft.EntityFrameworkCore;

namespace Benchmarks.Model
{

    public sealed class DatabaseContext : DbContext
    {
        private static DbContextOptions<DatabaseContext> _Options;

        private static DbContextOptions<DatabaseContext> _NoTrackingOptions;

        #region Factory

        public static DatabaseContext Create()
        {
            return new DatabaseContext(_Options ??= GetOptions(true));
        }

        public static DatabaseContext CreateNoTracking()
        {
            return new DatabaseContext(_NoTrackingOptions ??= GetOptions(false));
        }

        private static DbContextOptions<DatabaseContext> GetOptions(bool tracking)
        {
            var optionsBuilder = new DbContextOptionsBuilder<DatabaseContext>();

            optionsBuilder.UseNpgsql("Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=18;NoResetOnClose=true;Enlist=false;Max Auto Prepare=4");

            if (!tracking)
            {
                optionsBuilder.UseQueryTrackingBehavior(QueryTrackingBehavior.NoTracking);
            }

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
