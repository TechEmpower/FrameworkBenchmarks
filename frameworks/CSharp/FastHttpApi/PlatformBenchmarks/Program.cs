using BeetleX;
using BeetleX.Buffers;
using BeetleX.EventArgs;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using System;
using System.Threading;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    class Program
    {
        static void Main(string[] args)
        {
            var builder = new HostBuilder()
               .ConfigureServices((hostContext, services) =>
               {
                   services.AddHostedService<BeetleXServer>();
               });
            var result = builder.RunConsoleAsync();
            result.Wait();
        }
    }

    public class BeetleXServer : BackgroundService, BeetleX.IServerHandler
    {
        public virtual void Connected(IServer server, ConnectedEventArgs e)
        {
            if (server.EnableLog(LogType.Info))
                server.Log(LogType.Info, null, "session connected from {0}@{1}", e.Session.RemoteEndPoint, e.Session.ID);
        }

        public virtual void Connecting(IServer server, ConnectingEventArgs e)
        {
            e.Socket.NoDelay = true;
            if (server.EnableLog(LogType.Info))
                server.Log(LogType.Info, null, "connect from {0}", e.Socket.RemoteEndPoint);
        }

        public virtual void Disconnect(IServer server, SessionEventArgs e)
        {
            if (server.EnableLog(LogType.Info))
                server.Log(LogType.Info, null, "session {0}@{1} disconnected", e.Session.RemoteEndPoint, e.Session.ID);
        }

        public virtual void Error(IServer server, ServerErrorEventArgs e)
        {
            if (e.Session == null)
            {
                server.Log(LogType.Error, null, "server error {0}@{1}\r\n{2}", e.Message, e.Error.Message, e.Error.StackTrace);
            }
            else
            {
                server.Log(LogType.Error, null, "session {2}@{3} error {0}@{1}\r\n{4}", e.Message, e.Error.Message, e.Session.RemoteEndPoint, e.Session.ID, e.Error.StackTrace);
            }
        }

        public virtual void Log(IServer server, ServerLogEventArgs e)
        {
            Console.WriteLine("[{0}:{2}] {1}", e.Type, e.Message, DateTime.Now);
        }

        public virtual void SessionDetection(IServer server, SessionDetectionEventArgs e)
        {
            ;
        }

        public virtual void SessionPacketDecodeCompleted(IServer server, PacketDecodeCompletedEventArgs e)
        {

        }

        private void Onplaintext(PipeStream stream)
        {
            HttpParse.Plaintext(stream);
        }

        private void OnJson(PipeStream stream)
        {
            HttpParse.Json(stream);
        }

        public virtual void SessionReceive(IServer server, SessionReceiveEventArgs e)
        {
            PipeStream stream = e.Stream.ToPipeStream();
            string line = null;
            if (stream.TryReadWith(HttpParse.LineBytes, out line))
            {
                stream.ReadFree((int)stream.Length);
                Tuple<string, string, string> request = HttpParse.AnalyzeRequestLine(line);
                if (request.Item2 == "/plaintext")
                {
                    Onplaintext(stream);
                }
                else if (request.Item2 == "/json")
                {
                    OnJson(stream);
                }
                else if (request.Item2 == "/")
                {
                    stream.Write(HttpParse.BeetleXServerBytes, 0, HttpParse.BeetleXServerBytes.Length);
                }
                else
                {
                    stream.Write(HttpParse.NotFoundBytes, 0, HttpParse.NotFoundBytes.Length);
                }
                stream.Flush();
            }
            else
            {
                e.Session.Dispose();
            }

        }

        private BeetleX.IServer mServer;

        protected override async Task ExecuteAsync(CancellationToken stoppingToken)
        {
            NetConfig config = new NetConfig();
            config.Port = 8080;
            config.LogLevel = LogType.Warring;
            mServer = SocketFactory.CreateTcpServer(config, this, null);
            mServer.Open();
            Console.WriteLine("BeetleX base server");
            Console.WriteLine($"ServerGC:{System.Runtime.GCSettings.IsServerGC}");
            Console.Write(mServer);
            while (!stoppingToken.IsCancellationRequested)
            {
                Console.WriteLine("BeetleX is doing background work.");
                await Task.Delay(TimeSpan.FromSeconds(5), stoppingToken);
            }
        }
    }
}
