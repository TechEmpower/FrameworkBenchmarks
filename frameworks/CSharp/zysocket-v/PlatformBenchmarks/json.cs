using SpanJson;
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
            JsonSerializer.NonGeneric.Utf8.SerializeAsync(jsonMessage, write.Stream);
            OnCompleted(fiberRw, write);
        }
    }
}
