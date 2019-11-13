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

        public virtual async Task StartAsync(CancellationToken cancellationToken)
        {
            ArraySegment<byte> date = GMTDate.Default.DATE;
            ServerOptions serverOptions = new ServerOptions();
            serverOptions.LogLevel = LogType.Error;
            serverOptions.DefaultListen.Port = 8080;
            serverOptions.Statistical = false;
            serverOptions.BufferSize = 2048;
            serverOptions.BufferPoolMaxMemory = 1000;
            serverOptions.BufferPoolSize = 1024 * 4;
            mApiServer = SocketFactory.CreateTcpServer<HttpHandler>(serverOptions);
            mApiServer.Open();
            System.Net.Http.HttpClient client = new System.Net.Http.HttpClient();
            var response = await client.GetAsync("http://localhost:8080/json");
            mApiServer.Log(LogType.Info, null, $"Get josn {response.StatusCode}");
            response = await client.GetAsync("http://localhost:8080/plaintext");
            mApiServer.Log(LogType.Info, null, $"Get plaintext {response.StatusCode}");
            mApiServer.Log(LogType.Info, null, $"Debug mode [{Program.Debug}]");
        }

        public virtual Task StopAsync(CancellationToken cancellationToken)
        {
            mApiServer.Dispose();
            return Task.CompletedTask;
        }
    }
}
