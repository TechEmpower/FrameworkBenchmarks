using AkazawaYun.PRO7;

namespace AkazawaYun.FrameworkBenchmarks;

internal static class WorldService
{
    public static async Task<World> GetRandomWorld(IDb con, Dictionary<string, object?>? shared = null)
    {
        string sql = "SELECT id, randomNumber FROM world WHERE id = @id";
        int id = Random.Shared.Next(1, 10001);
        shared ??= new DpSingleBuilder().Build();
        shared["id"] = id;

        World? obj = await con.Find(sql, shared).ToWorld();
        return obj!;
    }
    public static async Task<World[]> GetWorlds(IDb con, int count)
    {
        World[] lst = new World[count];
        var dp = new DpSingleBuilder().Build();
        for (int i = 0; i < count; i++)
        {
            World obj = await GetRandomWorld(con, dp);
            lst[i] = obj;
        }
        return lst;
    }
    public static async Task SaveWorlds(IDb con, World[] lst)
    {
        string sql = "UPDATE world SET randomNumber=@randomNumber WHERE id=@id ;";
        await con.Execute(sql, lst.ToDp());
    }
}
