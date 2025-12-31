using Benchmarks.Tests;
using Benchmarks.Utilities;

#if INTERNAL
using GenHTTP.Engine.Internal;
#else
using GenHTTP.Engine.Kestrel;
#endif

using GenHTTP.Modules.IO;
using GenHTTP.Modules.Layouting;
using GenHTTP.Modules.Reflection;
using GenHTTP.Modules.Webservices;

var mode = ExecutionMode.Auto;

var tests = Layout.Create()
                  .Add("plaintext", Content.From(Resource.FromString("Hello, World!")))
                  .Add("json", new JsonHandler())
                  .Add("fortunes", new FortuneHandler())
                  .AddService<DbResource>("db", mode: mode)
                  .AddService<QueryResource>("queries", mode: mode)
                  .AddService<UpdateResource>("updates", mode: mode)
                  .AddService<CacheResource>("cached-worlds", mode: mode)
                  .Add(ServerHeader.Create());

return await Host.Create()
                 .Handler(tests)
                 .RunAsync();
