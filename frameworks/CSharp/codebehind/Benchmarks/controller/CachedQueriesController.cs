using CodeBehind;
using System.Text.Json;

public partial class cached__queries : CodeBehindController
{
    public async void PageLoad(HttpContext context)
    {
        context.Response.ContentType = "application/json";

        Write(await GetCachedResult(context));
    }

    private async Task<string> GetCachedResult(HttpContext context)
    {
        string Count = context.Request.Query["count"];

        ViewCache cache = new ViewCache(context);
        string CacheResult = cache.GetViewCache("cached_query_" + Count);
        if (cache.ViewHasCache)
            return CacheResult;

        CacheResult = LoadMultipleCachedQueryRow(Count.ToNumber());
        cache.SetViewCache("cached_query_" + Count, CacheResult, 3600);
        return CacheResult;
    }

    private string LoadMultipleCachedQueryRow(int Count)
    {
        Count = Count < 1 ? 1 : Count > 500 ? 500 : Count;

        var wr = new CachedWorldRow[Count];
        var dbc = new DatabaseContext();
        var random = new Random();

        for (int i = 0; i < Count; i++)
        {
            int RandomId = random.Next(1, 10001);
            var row = dbc.CachedWorld.Find(RandomId);
            wr[i] = row;
        }

        string json = JsonSerializer.Serialize(wr);

        return json;
    }
}
