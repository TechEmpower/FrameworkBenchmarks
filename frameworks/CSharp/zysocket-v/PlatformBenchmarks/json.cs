using Swifter.Json;
using System;
using System.Threading.Tasks;
using ZYSocket;
using ZYSocket.FiberStream;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {
        public void Json(IFiberRw<HttpToken> fiberRw,ref WriteBytes write)
        {
            JsonMessage jsonMessage = default(JsonMessage);
            jsonMessage.message = "Hello, World!";          
             JsonFormatter.SerializeObject(jsonMessage,write.Stream,System.Text.Encoding.UTF8);
            OnCompleted(fiberRw, write);
        }
    }
}
