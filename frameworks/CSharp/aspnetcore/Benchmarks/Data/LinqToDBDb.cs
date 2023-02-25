using LinqToDB;

namespace Benchmarks.Data;

public sealed class LinqToDBDb : IDb
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

    public Task<World> LoadSingleQueryRow()
    {
        return _firstWorldQuery(_dataConnection, _random.Next(1, 10001));
    }

    public async Task<World[]> LoadMultipleQueriesRows(int count)
    {
        var result = new World[count];

        for (var i = 0; i < count; i++)
        {
            result[i] = await _firstWorldQuery(_dataConnection, _random.Next(1, 10001));
        }

        return result;
    }

    public async Task<World[]> LoadMultipleUpdatesRows(int count)
    {
        var results = new World[count];

        for (var i = 0; i < count; i++)
        {
            results[i] = await _dataConnection.World.FirstAsync(w => w.Id == _random.Next(1, 10001));
            results[i].RandomNumber = _random.Next(1, 10001);
        }

        await (from w in _dataConnection.World
               join r in results on w.Id equals r.Id
               select new { w, r })
            .Set(u => u.w.RandomNumber, u => u.r.RandomNumber)
            .UpdateAsync();

        return results;
    }

    public async Task<List<Fortune>> LoadFortunesRows()
    {
        var result = await _dataConnection.Fortune.ToListAsync();

        result.Add(new Fortune { Message = "Additional fortune added at request time." });
        result.Sort();

        return result;
    }
}