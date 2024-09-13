using CodeBehind;
using System.Text.Json;

public partial class updates : CodeBehindController
{
    public async void PageLoad(HttpContext context)
    {
        context.Response.ContentType = "application/json";
        int Count = context.Request.Query["queries"].ToNumber();

        Write(await LoadUpdatesQueryRow(Count));
    }

    private async Task<string> LoadUpdatesQueryRow(int Count)
    {
        Count = Count < 1 ? 1 : Count > 500 ? 500 : Count;

        var wr = new WorldRow[Count];
        var dbc = new DatabaseContext();

        for (int i = 0; i < Count; i++)
        {
            int RandomId = Random.Shared.Next(1, 10001);
            var row = dbc.World.Find(RandomId);
            row.randomNumber = Random.Shared.Next(1, 10001);
            wr[i] = row;
        }

        dbc.SaveChangesAsync();

        string json = JsonSerializer.Serialize(wr);

        return json;
    }
}
