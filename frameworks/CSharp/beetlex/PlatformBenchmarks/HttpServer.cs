using BeetleX;
using BeetleX.EventArgs;
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
        public static IServer ApiServer;

        public virtual Task StartAsync(CancellationToken cancellationToken)
        {
            ArraySegment<byte> date = GMTDate.Default.DATE;
            ServerOptions serverOptions = new ServerOptions();
            serverOptions.LogLevel = LogType.Error;
            serverOptions.DefaultListen.Port = 8080;
            serverOptions.Statistical = false;
            serverOptions.BufferPoolMaxMemory = 1000;
            serverOptions.BufferPoolSize = 1024 * 24;
            ApiServer = SocketFactory.CreateTcpServer<HttpHandler>(serverOptions);
            ApiServer.Open();
            if (!Program.UpDB)
            {
               RawDb._connectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=256;NoResetOnClose=true;Enlist=false;Max Auto Prepare=4;Multiplexing=true;Write Coalescing Delay Us=500;Write Coalescing Buffer Threshold Bytes=1000";
               // RawDb._connectionString = "Server=192.168.2.19;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=256;NoResetOnClose=true;Enlist=false;Max Auto Prepare=3";
            }
            else
            {

                RawDb._connectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=64;NoResetOnClose=true;Enlist=false;Max Auto Prepare=3;Multiplexing=true;Write Coalescing Delay Us=500;Write Coalescing Buffer Threshold Bytes=1000";
               // RawDb._connectionString = "Server=192.168.2.19;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=64;NoResetOnClose=true;Enlist=false;Max Auto Prepare=3";
            }
           // ApiServer.Log(LogType.Info, null, $"Debug mode [{Program.Debug}]");
            return Task.CompletedTask;

        }

        public virtual Task StopAsync(CancellationToken cancellationToken)
        {
            ApiServer.Dispose();
            return Task.CompletedTask;
        }
    }
}
