using BeetleX.FastHttpApi;
using System;

namespace Benchmarks
{
    [BeetleX.FastHttpApi.Controller]
    class Program
    {
        private static HttpApiServer mApiServer;

        private static byte[] plaintextData;

        private static byte[] jsonData;

        static void Main(string[] args)
        {
            mApiServer = new HttpApiServer();
            mApiServer.Register(typeof(Program).Assembly);
            mApiServer.ServerConfig.Port = 8080;
            mApiServer.ServerConfig.UrlIgnoreCase = false;
            mApiServer.ServerConfig.LogLevel = BeetleX.EventArgs.LogType.Info;
            mApiServer.ServerConfig.LogToConsole = true;
            mApiServer.Open();
            Console.WriteLine($"ServerGC:{System.Runtime.GCSettings.IsServerGC}");
            Console.Write(mApiServer.BaseServer);
        }

        public object plaintext(IHttpContext context)
        {
            context.Response.Header[HeaderTypeFactory.DATE] = DateTime.Now.ToUniversalTime().ToString("r");
            return new TextResult("Hello, World!");
        }
        public object json(IHttpContext context)
        {
            context.Response.Header[HeaderTypeFactory.DATE] = DateTime.Now.ToUniversalTime().ToString("r");
            return new JsonResult(new JsonMessage { message = "Hello, World!" });
        }

        public class JsonMessage
        {
            public string message { get; set; }
        }
    }

}
