using AkazawaYun.PRO7;

namespace AkazawaYun.FrameworkBenchmarks;

class WorldService
{
    static readonly int @id;
    // SELECT id, randomNumber FROM world WHERE id=@id ;
    public static readonly string SqlSelect = akzSqlinq<world>.Query().Select(m => new
    {
        m.id,
        m.randomNumber,
    }).Where(m => m.id == @id).Build();
    // UPDATE world SET randomNumber=@randomNumber WHERE id=@id ;
    public static readonly string SqlUpdate = akzSqlinq<world>.Update().Set(m => new()
    {
        randomNumber = m.randomNumber,
    }).Where(m => m.id == @id).Build();


    public static async Task<world> GetRandomWorld(IDb con, IDictionary<string, object?>? shared = null)
    {
        int id = Random.Shared.Next(1, 10001);
        shared ??= new DpSingleBuilder().Build();
        shared["id"] = id;

        world? obj = await con.Find(SqlSelect, shared).Toworld();
        return obj!;
    }
    public static async Task<world[]> GetWorlds(IDb con, int count)
    {
        world[] lst = new world[count];
        var dp = new DpSingleBuilder().Build();
        for (int i = 0; i < count; i++)
        {
            world obj = await GetRandomWorld(con, dp);
            lst[i] = obj;
        }
        return lst;
    }
    public static async Task SaveWorlds(IDb con, world[] lst)
    {
        await con.Execute(SqlUpdate, lst.ToDp());
    }
}
