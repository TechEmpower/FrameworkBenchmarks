using Framework;

try
{
    WebFramework server = new("config.json");

    server.Start(true, () => Console.WriteLine("Server is running..."));
}
catch (Exception e)
{
    Console.WriteLine(e.Message);

    throw;
}
