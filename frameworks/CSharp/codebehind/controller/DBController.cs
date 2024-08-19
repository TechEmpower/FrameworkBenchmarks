using CodeBehind;
using System.Text.Json;

public partial class DBController : CodeBehindController
{
    public async void PageLoad(HttpContext context)
    {
        context.Response.ContentType = "application/json";

        Write(await LoadSingleQueryRow());
    }

    private async Task<string> LoadSingleQueryRow()
    {
        var dbc = new DatabaseContext();
        //var row = new WorldRow { randomNumber = new Random().Next(1, 10001)};
        //dbc.World.Add(row);
        //dbc.SaveChanges();

        int RandomId = Random.Shared.Next(1, 10001);
        var world = dbc.World.Find(RandomId);
        string json = JsonSerializer.Serialize(world);

        return json;
    }
}