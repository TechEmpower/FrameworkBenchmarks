using Swifter.Json;
using System;
using System.Threading.Tasks;
using ZYSocket;
using ZYSocket.FiberStream;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {
        public async void Json(IFiberRw<HttpToken> fiberRw,WriteBytes write)
        {
            JsonMessage jsonMessage = default(JsonMessage);
            jsonMessage.message = "Hello, World!";          
            JsonFormatter.SerializeObject(jsonMessage,write.Stream,System.Text.Encoding.UTF8);
            var length = write.Stream.Length - fiberRw.UserToken.HttpHandlerPostion;
            write.Stream.Position = fiberRw.UserToken.ContentPostion.postion;
            write.Write(length.ToString(), false);
            await write.Flush();
        }
    }
}
