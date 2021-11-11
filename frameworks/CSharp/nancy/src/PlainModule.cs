namespace Nancy.Benchmark
{
    public class PlainModule : NancyModule
    {
        public PlainModule() : base("plaintext")
        {
            Get("/", args =>
            {
                return Response.AsText("Hello, World!");
            });
        }
    }
}