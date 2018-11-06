using BeetleX.FastHttpApi;
using System;

namespace Benchmarks
{
    [BeetleX.FastHttpApi.Controller]
    class Program
    {
        private static HttpApiServer mApiServer;

        static void Main(string[] args)
        {
            mApiServer = new HttpApiServer();
            mApiServer.Register(typeof(Program).Assembly);
            mApiServer.ServerConfig.Port = 8080;
            mApiServer.ServerConfig.UrlIgnoreCase = false;
            mApiServer.Open();
            Console.WriteLine($"ServerGC:{System.Runtime.GCSettings.IsServerGC}");
            Console.Write(mApiServer.BaseServer);
            Console.Read();
        }

        public object plaintext()
        {
            return new TextResult("Hello, World!");
        }
        public object json()
        {
            return new JsonResult("Hello, World!");
        }
    }
}
