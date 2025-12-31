using Benchmarks;

using Unhinged;
using Unhinged.GenHttp.Experimental;

var project = Project.Create();

UnhingedEngine.CreateBuilder()
              .SetPort(8080)
              .SetNWorkersSolver(() => Environment.ProcessorCount - 2)
              .SetBacklog(16384)
              .SetMaxEventsPerWake(512)
              .SetMaxNumberConnectionsPerWorker(1024)
              .SetSlabSizes(32 * 1024, 16 * 1024)
              .Map(project)
              .Build()
              .Run();
