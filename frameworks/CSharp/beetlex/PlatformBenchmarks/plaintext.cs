using BeetleX;
using BeetleX.Buffers;
using System;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {
        private readonly static AsciiString _plaintextPreamble =

           _headerContentLength + _result_plaintext.Length.ToString();

        public ValueTask Plaintext(PipeStream stream, HttpToken token, ISession session)
        {
            stream.Write(_result_plaintext.Data, 0, _result_plaintext.Length);
            OnCompleted(stream, session, token);
            return ValueTask.CompletedTask;
        }
    }
}
