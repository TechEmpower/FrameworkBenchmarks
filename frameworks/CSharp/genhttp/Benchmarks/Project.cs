using Benchmarks.Tests;
using Benchmarks.Utilities;

using GenHTTP.Api.Content;

using GenHTTP.Modules.IO;
using GenHTTP.Modules.Layouting;
using GenHTTP.Modules.Reflection;
using GenHTTP.Modules.Webservices;

namespace Benchmarks;

public static class Project
{

    public static IHandlerBuilder Create()
    {
        var mode = ExecutionMode.Auto;

        return Layout.Create()
                     .Add("plaintext", Content.From(Resource.FromString("Hello, World!")))
                     .Add("json", new JsonHandler())
                     .Add("fortunes", new FortuneHandler())
                     .AddService<DbResource>("db", mode: mode)
                     .AddService<QueryResource>("queries", mode: mode)
                     .AddService<UpdateResource>("updates", mode: mode)
                     .AddService<CacheResource>("cached-worlds", mode: mode)
                     .Add(ServerHeader.Create());
    }

}
