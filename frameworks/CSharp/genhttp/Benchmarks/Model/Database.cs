namespace Benchmarks.Model;

public static class Database
{
    public static readonly DatabaseContextPool<DatabaseContext> NoTrackingPool;

    public static readonly DatabaseContextPool<DatabaseContext> TrackingPool;

    static Database()
    {
        NoTrackingPool = new DatabaseContextPool<DatabaseContext>(factory: DatabaseContext.CreateNoTracking, maxSize: 512);
        TrackingPool = new DatabaseContextPool<DatabaseContext>(factory: DatabaseContext.CreateTracking, maxSize: 512);
    }

}
