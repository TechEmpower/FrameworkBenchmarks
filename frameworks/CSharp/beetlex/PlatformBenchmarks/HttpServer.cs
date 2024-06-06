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

        public virtual Task StartAsync(CancellationToken cancellationToken)
        {
            Constants.MemorySegmentMinSize = 1024 * 8;
            Constants.MemorySegmentMaxSize = 1024 * 8;
            ArraySegment<byte> date = GMTDate.Default.DATE;
            _apiServer = new NetServer<HttpNetApplication, HttpHandler>();
            _apiServer.Options.LogLevel = BeetleX.Light.Logs.LogLevel.Trace;
            _apiServer.Options.AddLogOutputHandler<LogOutputToConsole>();
            _apiServer.Options.SetDefaultListen(o =>
            {
                o.Port = 8080;
            });
            _apiServer.Start();
            //ServerOptions serverOptions = new ServerOptions();
            //serverOptions.LogLevel = LogType.Error;
            //serverOptions.DefaultListen.Port = 8080;
            //serverOptions.Statistical = false;
            //serverOptions.BufferPoolMaxMemory = 1000;
            //serverOptions.BufferPoolSize = 1024 * 24;
            //ApiServer = SocketFactory.CreateTcpServer<HttpHandler>(serverOptions);
            //ApiServer.Open();

            if (!Program.UpDB)
            {
                //RawDb._connectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=256;NoResetOnClose=true;Enlist=false;Max Auto Prepare=4;Multiplexing=true;Write Coalescing Delay Us=500;Write Coalescing Buffer Threshold Bytes=1000";
                RawDb._connectionString = "Server=127.0.0.1;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=256;NoResetOnClose=true;Enlist=false;Max Auto Prepare=3";
            }
            else
            {

                // RawDb._connectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=64;NoResetOnClose=true;Enlist=false;Max Auto Prepare=3;Multiplexing=true;Write Coalescing Delay Us=500;Write Coalescing Buffer Threshold Bytes=1000";
                RawDb._connectionString = "Server=127.0.0.1;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=64;NoResetOnClose=true;Enlist=false;Max Auto Prepare=3";
            }
            // ApiServer.Log(LogType.Info, null, $"Debug mode [{Program.Debug}]");
            return Task.CompletedTask;

        }

        public virtual Task StopAsync(CancellationToken cancellationToken)
        {

            return Task.CompletedTask;
        }
    }
}
