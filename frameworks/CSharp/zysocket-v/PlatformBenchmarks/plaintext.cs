using System;
using System.Threading.Tasks;
using ZYSocket;
using ZYSocket.FiberStream;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {
        public async void Plaintext( IFiberRw<HttpToken> fiberRw,  WriteBytes write)
        {
            write.Write(_result_plaintext.Data, 0, _result_plaintext.Length);
            var length = write.Stream.Length - fiberRw.UserToken.HttpHandlerPostion;
            write.Stream.Position = fiberRw.UserToken.ContentPostion.postion;
            write.Write(length.ToString(), false);
            await write.Flush();          
        }
    }
}
