using Benchmarks.Tests;
using Benchmarks.Utilities;

using GenHTTP.Engine.Kestrel;
using GenHTTP.Modules.IO;
using GenHTTP.Modules.Layouting;
using GenHTTP.Modules.Webservices;

var tests = Layout.Create()
                  .Add("plaintext", Content.From(Resource.FromString("Hello, World!")))
                  .Add("fortunes", new FortuneHandler())
                  .AddService<JsonResource>("json")
                  .AddService<DbResource>("db")
                  .AddService<QueryResource>("queries")
                  .AddService<UpdateResource>("updates")
                  .AddService<CacheResource>("cached-worlds")
                  .Add(ServerHeader.Create());

return await Host.Create()
                 .Handler(tests)
                 .RunAsync();
