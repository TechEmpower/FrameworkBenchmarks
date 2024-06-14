using BeetleX.Light;
using BeetleX.Light.Logs;
using Microsoft.Extensions.Hosting;
using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public class HttpServer : IHostedService
    {
        private static NetServer<HttpNetApplication, HttpHandler> _apiServer;

        public static string _connectionString;

        public virtual Task StartAsync(CancellationToken cancellationToken)
        {
            _connectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;SSL Mode=Disable;Maximum Pool Size=16;NoResetOnClose=true;Enlist=false;Max Auto Prepare=4;Multiplexing=true;Write Coalescing Buffer Threshold Bytes=1000";
            //_connectionString = "Server=localhost;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;SSL Mode=Disable;Maximum Pool Size=16;NoResetOnClose=true;Enlist=false;Max Auto Prepare=4;Multiplexing=true;Write Coalescing Buffer Threshold Bytes=1000";
            ThreadPool.SetMinThreads(Environment.ProcessorCount * 2, Environment.ProcessorCount * 2);
            Constants.MemorySegmentMinSize = 1024 * 16;
            Constants.MemorySegmentMaxSize = 1024 * 16;
            Constants.InitMemoryBlock();
            var date = GMTDate.Default.DATE;
            _apiServer = new NetServer<HttpNetApplication, HttpHandler>();
            _apiServer.Options.LogLevel = BeetleX.Light.Logs.LogLevel.Trace;
            _apiServer.Options.AddLogOutputHandler<LogOutputToConsole>();
            _apiServer.Options.SetDefaultListen(o =>
            {
                o.Port = 8080;
            });
            _apiServer.Start();

            return Task.CompletedTask;

        }

        public virtual Task StopAsync(CancellationToken cancellationToken)
        {

            return Task.CompletedTask;
        }
    }
}
