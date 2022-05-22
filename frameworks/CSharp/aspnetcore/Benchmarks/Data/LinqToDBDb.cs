using LinqToDB;

namespace Benchmarks.Data;

public class LinqToDBDb : IDb
{
    private readonly IRandom _random;
    private readonly ApplicationDataConnection _dataConnection;

    public LinqToDBDb(IRandom random, ApplicationDataConnection dataConnection)
    {
        _random = random;

        _dataConnection = dataConnection;
    }

    private static readonly Func<ApplicationDataConnection, int, Task<World>> _firstWorldQuery
        = CompiledQuery.Compile((ApplicationDataConnection db, int id)
            => db.World.FirstAsync(w => w.Id == id, default));

    private Task<World> FirstWorldQuery(int id)
    {
        return _firstWorldQuery(_dataConnection, id);
    }

    public Task<World> LoadSingleQueryRow()
    {
        var id = _random.Next(1, 10001);

        return FirstWorldQuery(id);
    }

    public async Task<World[]> LoadMultipleQueriesRows(int count)
    {
        var result = new World[count];

        for (var i = 0; i < count; i++)
        {
            var id = _random.Next(1, 10001);

            result[i] = await FirstWorldQuery(id);
        }

        return result;
    }

    public async Task<World[]> LoadMultipleUpdatesRows(int count)
    {
        var results = new World[count];
        var usedIds = new HashSet<int>(count);

        for (var i = 0; i < count; i++)
        {
            int id;
            do
            {
                id = _random.Next(1, 10001);
            } while (!usedIds.Add(id));

            results[i] = await FirstWorldQuery(id);

            results[i].RandomNumber = _random.Next(1, 10001);
        }

        await (from w in _dataConnection.World
               join r in results on w.Id equals r.Id
               select new { w, r })
            .Set(u => u.w.RandomNumber, u => u.r.RandomNumber)
            .UpdateAsync();

        return results;
    }

    private static readonly Func<ApplicationDataConnection, IAsyncEnumerable<Fortune>> _fortunesQuery
        = CompiledQuery.Compile((ApplicationDataConnection db) => db.Fortune.AsAsyncEnumerable());

    public async Task<List<Fortune>> LoadFortunesRows()
    {
        var result = new List<Fortune>();

        await foreach (var element in _fortunesQuery(_dataConnection))
        {
            result.Add(element);
        }

        result.Add(new Fortune { Message = "Additional fortune added at request time." });
        result.Sort();

        return result;
    }
}