using System;
using System.Net;
using System.Net.Sockets;

using NetCoreServer;

namespace Benchmarks
{

    public class HttpBenchmarkServer : HttpServer
    {

        public HttpBenchmarkServer(IPAddress address, int port) : base(address, port) { }

        protected override TcpSession CreateSession() { return new HttpBenchmarkSession(this); }

        protected override void OnError(SocketError error)
        {
            Console.WriteLine($"HTTP session caught an error: {error}");
        }

    }

}
