namespace AkazawaYun.Benchmark.Aot;

internal class Program
{
    static async Task Main(string[] args)
    {
        AotImport.ServerLaunch(args.Length);
        await Task.Delay(-1);
    }

    public static string plaintext() => "Hello, World!";
    public static JsonModel json() => new()
    {
        message = "Hello, World!"
    };
    public class JsonModel
    {
        public string? message { get; set; }
    }

}
