using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using ZYSocket;
using ZYSocket.Server;
using ZYSocket.Server.Builder;

namespace PlatformBenchmarks
{
    public class ZYHttpServer : IHostedService
    {
        public ISocketServer SocketServer { get; private set; }
        public IServiceProvider serviceProvider { get; private set; }

        public HttpHandler @HttpHandler { get; private set; }

        public ZYHttpServer()
        {
            ArraySegment<byte> date = GMTDate.Default.DATE;
            var containerBuilder = new ServiceCollection();
            new SockServBuilder(containerBuilder, p =>
            {
                return new ZYSocketSuper(p)
                {
                    BinaryInput = new BinaryInputHandler(BinaryInputHandler),
                    Connetions = new ConnectionFilter(ConnectionFilter),
                    MessageInput = new DisconnectHandler(DisconnectHandler)
                };
            })
             .ConfigServer(p =>
             {
                 p.Port = 8080;
                 p.MaxBufferSize = 1024;
                 p.MaxConnectCout = 20000;
             });

            serviceProvider = containerBuilder.BuildServiceProvider();
            SocketServer = serviceProvider.GetRequiredService<ISocketServer>();
            @HttpHandler = new HttpHandler();
        }


        public Task StartAsync(CancellationToken cancellationToken)
        {
            SocketServer.Start();
            return Task.CompletedTask;
        }

        public Task StopAsync(CancellationToken cancellationToken)
        {
            SocketServer.Stop();
            return Task.CompletedTask;
        }

        bool ConnectionFilter(ISockAsyncEvent socketAsync) => true;

        void DisconnectHandler(string message, ISockAsyncEvent socketAsync, int erorr)
        {
            socketAsync.UserToken = null;
            socketAsync.AcceptSocket.Dispose();
        }


        async void BinaryInputHandler(ISockAsyncEvent socketAsync)
        {
            var fiberRw = await socketAsync.GetFiberRw<HttpToken>();
            fiberRw.UserToken = new HttpToken
            {
                Db = new RawDb(new ConcurrentRandom(), Npgsql.NpgsqlFactory.Instance)
            };

            using var data_r = fiberRw.GetMemory(1024);         
            using var write = new WriteBytes(fiberRw);
            for (; ; )
            {
                await HttpHandler.Receive(fiberRw, data_r.Memory, write);
            }

        }
    }
}
