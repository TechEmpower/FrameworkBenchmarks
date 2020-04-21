using GenHTTP.Core;
using GenHTTP.Modules.Core;
using GenHTTP.Modules.Webservices;

using Benchmarks.Tests;

namespace Benchmarks
{

    public static class Program
    {

        public static int Main(string[] args)
        {
            var tests = Layout.Create()
                              .Add("plaintext", Content.From("Hello, World!"))
                              .Add<JsonResource>("json");

            return Host.Create()
                       .Handler(tests)
                       .Run();
        }

    }

}
