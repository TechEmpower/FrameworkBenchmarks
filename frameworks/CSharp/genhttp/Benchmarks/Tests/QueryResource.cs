using Benchmarks.Model;
using GenHTTP.Modules.Webservices;
using Microsoft.EntityFrameworkCore;

namespace Benchmarks.Tests;

public sealed class QueryResource
{
    private static readonly Random Random = new();

    [ResourceMethod(":queries")]
    public ValueTask<List<World>> GetWorldsFromPath(string queries) => GetWorlds(queries);

    [ResourceMethod]
    public async ValueTask<List<World>> GetWorlds(string queries)
    {
        var count = 1;

        int.TryParse(queries, out count);

        if (count < 1)
        {
            count = 1;
        }
        else if (count > 500)
        {
            count = 500;
        }

        var result = new List<World>(count);

        using var context = DatabaseContext.CreateNoTracking();

        for (var _ = 0; _ < count; _++)
        {
            var id = Random.Next(1, 10001);

            result.Add(await context.World.FirstOrDefaultAsync(w => w.Id == id).ConfigureAwait(false));
        }

        return result;
    }

}