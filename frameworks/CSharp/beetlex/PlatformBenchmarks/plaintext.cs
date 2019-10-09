using BeetleX;
using BeetleX.Buffers;
using System;
using System.Collections.Generic;
using System.Text;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {
        public void Plaintext(ReadOnlySpan<byte> url, PipeStream stream, HttpToken token, ISession session)
        {
            stream.Write(_result_plaintext.Data, 0, _result_plaintext.Length);
            OnCompleted(stream, session, token);
        }
    }
}
