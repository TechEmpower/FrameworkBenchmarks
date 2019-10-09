using System;
using ZYSocket;
using ZYSocket.FiberStream;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {
        public  void Plaintext( IFiberRw<HttpToken> fiberRw,  ref WriteBytes write)
        {
            write.Write(_result_plaintext.Data, 0, _result_plaintext.Length);
            OnCompleted(fiberRw, write);
        }
    }
}
