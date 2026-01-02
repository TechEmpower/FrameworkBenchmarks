using Benchmarks;

using GenHTTP.Engine.Kestrel;

var project = Project.Create();

return await Host.Create()
                 .Handler(project)
                 .RunAsync();
