using Benchmarks;

using GenHTTP.Adapters.WiredIO;

using Wired.IO.App;

var project = Project.Create();

await WiredApp.CreateExpressBuilder()
              .Port(8080)
              .MapGenHttp("*", project)
              .Build()
              .RunAsync();
              