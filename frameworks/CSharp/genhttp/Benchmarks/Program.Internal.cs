using Benchmarks;

using GenHTTP.Engine.Internal;

var project = Project.Create();

return await Host.Create()
                 .Handler(project)
                 .RunAsync();
