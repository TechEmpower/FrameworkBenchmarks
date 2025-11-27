using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;

namespace Benchmarks.Model;

public sealed class DatabaseContext : DbContext
{
    private static readonly Lazy<DbContextOptions<DatabaseContext>> Options = new(CreateOptions, LazyThreadSafetyMode.ExecutionAndPublication);

    public static DatabaseContext Create() => new(Options.Value);

    private static DbContextOptions<DatabaseContext> CreateOptions()
    {
        var services = new ServiceCollection();

        services.AddEntityFrameworkNpgsql();

        var provider = services.BuildServiceProvider();

        var builder = new DbContextOptionsBuilder<DatabaseContext>();

        builder.UseInternalServiceProvider(provider)
               .UseNpgsql("Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;SSL Mode=Disable;Maximum Pool Size=512;NoResetOnClose=true;Enlist=false;Max Auto Prepare=4;Multiplexing=true")
               .UseQueryTrackingBehavior(QueryTrackingBehavior.NoTracking)
               .EnableThreadSafetyChecks(false)
               .UseModel(DatabaseContextModel.Instance);

        return builder.Options;
    }

    internal DatabaseContext(DbContextOptions<DatabaseContext> options) : base(options)
    {
        ChangeTracker.AutoDetectChangesEnabled = false;
    }

    public DbSet<World> World { get; set; }

    public DbSet<Fortune> Fortune { get; set; }

}
