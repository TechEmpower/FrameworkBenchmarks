namespace src;

using Celerio;
using static Celerio.Result;

public static class Endpoints
{
    [Get("/plaintext")]
    public static Result Plaintext()
    {
        return Ok().Text("Hello, World!");
    }

    [Serializable]
    public class SampleResponse
    {
        public string message { get; set; } = "Hello, World!";
    }
    
    [Get("/json")]
    public static Result Json()
    {
        return Ok().Json(new SampleResponse());
    }
}