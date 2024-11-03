﻿using Benchmarks.Tests;
using Benchmarks.Utilities;
using GenHTTP.Engine;
using GenHTTP.Modules.IO;
using GenHTTP.Modules.Layouting;
using GenHTTP.Modules.Webservices;

namespace Benchmarks;

public static class Program
{

    public static int Main()
    {
        var tests = Layout.Create()
                          .Add("plaintext", Content.From(Resource.FromString("Hello, World!")))
                          .Add("fortunes", new FortuneHandlerBuilder())
                          .AddService<JsonResource>("json")
                          .AddService<DbResource>("db")
                          .AddService<QueryResource>("queries")
                          .AddService<UpdateResource>("updates")
                          .AddService<CacheResource>("cached-worlds")
                          .Add(ServerHeader.Create());

        return Host.Create()
                   .Handler(tests)
                   .Run();
    }

}
