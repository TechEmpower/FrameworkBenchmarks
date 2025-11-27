using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;

namespace Benchmarks.Model;

public sealed class DatabaseContext : DbContext
{
    private static readonly Lazy<DbContextOptions<DatabaseContext>> TrackingOptions = new(() => CreateOptions(true), LazyThreadSafetyMode.ExecutionAndPublication);

    private static readonly Lazy<DbContextOptions<DatabaseContext>> NoTrackingOptions = new(() => CreateOptions(false), LazyThreadSafetyMode.ExecutionAndPublication);

    public static DatabaseContext CreateTracking() => new(TrackingOptions.Value, true);

    public static DatabaseContext CreateNoTracking() => new(NoTrackingOptions.Value, false);

    private static DbContextOptions<DatabaseContext> CreateOptions(bool tracking)
    {
        var services = new ServiceCollection();

        services.AddEntityFrameworkNpgsql();

        var provider = services.BuildServiceProvider();

        var builder = new DbContextOptionsBuilder<DatabaseContext>();

        builder.UseInternalServiceProvider(provider)
               .UseNpgsql("Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;SSL Mode=Disable;Maximum Pool Size=512;NoResetOnClose=true;Enlist=false;Max Auto Prepare=4;Multiplexing=true")
               .EnableThreadSafetyChecks(false)
               .UseModel(DatabaseContextModel.Instance);

        if (!tracking)
        {
            builder.UseQueryTrackingBehavior(QueryTrackingBehavior.NoTracking);
        }

        return builder.Options;
    }

    internal DatabaseContext(DbContextOptions<DatabaseContext> options, bool tracking = false) : base(options)
    {
        ChangeTracker.AutoDetectChangesEnabled = tracking;
    }

    public DbSet<World> World { get; set; }

    public DbSet<Fortune> Fortune { get; set; }

}
