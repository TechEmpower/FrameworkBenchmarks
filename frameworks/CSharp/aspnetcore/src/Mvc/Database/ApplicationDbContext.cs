using Microsoft.EntityFrameworkCore;
using Mvc.Models;

namespace Mvc.Database;

public sealed class ApplicationDbContext : DbContext
{
    public ApplicationDbContext(DbContextOptions options)
        : base(options)
    {
        ChangeTracker.QueryTrackingBehavior = QueryTrackingBehavior.NoTracking;
        ChangeTracker.AutoDetectChangesEnabled = false;
    }

    public required DbSet<Fortune> Fortunes { get; set; }

    public required DbSet<World> Worlds { get; set; }
}
