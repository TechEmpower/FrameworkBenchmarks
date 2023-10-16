using Microsoft.EntityFrameworkCore;
using Mvc.Models;

namespace Mvc.Database;

public sealed class Db
{
    private readonly ApplicationDbContext _dbContext;

    public Db(ApplicationDbContext dbContext)
    {
        _dbContext = dbContext;
    }

    private static readonly Func<ApplicationDbContext, int, Task<World>> _firstWorldQuery
        = EF.CompileAsyncQuery((ApplicationDbContext context, int id)
            => context.Worlds.First(w => w.Id == id));

    public Task<World> LoadSingleQueryRow()
    {
        var id = Random.Shared.Next(1, 10001);

        return _firstWorldQuery(_dbContext, id);
    }

    public async Task<World[]> LoadMultipleQueriesRows(int count)
    {
        count = count < 1 ? 1 : count > 500 ? 500 : count;

        var result = new World[count];

        for (var i = 0; i < count; i++)
        {
            var id = Random.Shared.Next(1, 10001);

            result[i] = await _firstWorldQuery(_dbContext, id);
        }

        return result;
    }

    private static readonly Func<ApplicationDbContext, int, Task<World>> _firstWorldTrackedQuery
        = EF.CompileAsyncQuery((ApplicationDbContext context, int id)
            => context.Worlds.AsTracking().First(w => w.Id == id));

    public async Task<World[]> LoadMultipleUpdatesRows(int count)
    {
        count = count < 1 ? 1 : count > 500 ? 500 : count;

        var results = new World[count];

        for (var i = 0; i < count; i++)
        {
            var id = Random.Shared.Next(1, 10001);
            var result = await _firstWorldTrackedQuery(_dbContext, id);
            result.RandomNumber = Random.Shared.Next(1, 10001);

            // Per the rules, always send an update, even if the random new value is the same as the current value.
            _dbContext.Entry(result).State = EntityState.Modified;
            
            results[i] = result;
        }

        await _dbContext.SaveChangesAsync();

        return results;
    }

    private static readonly Func<ApplicationDbContext, IAsyncEnumerable<Fortune>> _fortunesQuery
        = EF.CompileAsyncQuery((ApplicationDbContext context) => context.Fortunes);

    public async Task<IEnumerable<Fortune>> LoadFortunesRows()
    {
        var result = new List<Fortune>();

        await foreach (var fortune in _fortunesQuery(_dbContext))
        {
            result.Add(fortune);
        }

        result.Add(new Fortune { Message = "Additional fortune added at request time." });

        result.Sort();

        return result;
    }
}
