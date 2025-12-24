using Microsoft.EntityFrameworkCore;

namespace Benchmarks.Model;

public sealed class DatabaseContext : DbContext
{
    private static DbContextOptions<DatabaseContext> _options;

    private static DbContextOptions<DatabaseContext> _noTrackingOptions;

    #region Factory

    public static DatabaseContext Create() => new(_options ??= GetOptions(true));

    public static DatabaseContext CreateNoTracking() => new(_noTrackingOptions ??= GetOptions(false));

    private static DbContextOptions<DatabaseContext> GetOptions(bool tracking)
    {
        var optionsBuilder = new DbContextOptionsBuilder<DatabaseContext>();

        optionsBuilder.UseNpgsql("Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;SSL Mode=Disable;Maximum Pool Size=512;NoResetOnClose=true;Enlist=false;Max Auto Prepare=4");

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
