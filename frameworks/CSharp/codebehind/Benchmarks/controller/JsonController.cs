using CodeBehind;
using System.Text.Json;

public partial class JsonController : CodeBehindController
{
    public void PageLoad(HttpContext context)
    {
        context.Response.ContentType = "application/json";

        MyJsonObject MyObject = new MyJsonObject { message = "Hello, World!" };
        string json = JsonSerializer.Serialize(MyObject);

        Write(json);
    }
}

public class MyJsonObject
{
    public string message { get; set; }
}