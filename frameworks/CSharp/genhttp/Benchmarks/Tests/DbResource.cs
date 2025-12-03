using Benchmarks.Model;
using GenHTTP.Modules.Webservices;
using Microsoft.EntityFrameworkCore;

namespace Benchmarks.Tests;

public sealed class DbResource
{
    private static readonly Random Random = new();

    [ResourceMethod]
    public async ValueTask<World> GetRandomWorld()
    {
        var id = Random.Next(1, 10001);

        var context = Database.NoTrackingPool.Rent();

        try
        {
            return await context.World.FirstOrDefaultAsync(w => w.Id == id).ConfigureAwait(false);
        }
        finally
        {
            Database.NoTrackingPool.Return(context);
        }
    }

}
