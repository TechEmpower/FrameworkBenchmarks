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
        private IServer mApiServer;

        public virtual Task StartAsync(CancellationToken cancellationToken)
        {
            ArraySegment<byte> date = GMTDate.Default.DATE;
            ServerOptions serverOptions = new ServerOptions();
            serverOptions.LogLevel = LogType.Error;
            serverOptions.DefaultListen.Port = 8080;
            serverOptions.Statistical = false;
            serverOptions.PrivateBufferPool = true;
            serverOptions.MaxConnections = 100000;
            serverOptions.BufferPoolMaxMemory = 500;
            mApiServer = SocketFactory.CreateTcpServer<HttpHandler>(serverOptions);
            mApiServer.Open();
            return Task.CompletedTask;
        }

        public virtual Task StopAsync(CancellationToken cancellationToken)
        {
            mApiServer.Dispose();
            return Task.CompletedTask;
        }
    }
}
