using BeetleX;
using BeetleX.Buffers;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {
        private readonly static AsciiString _defaultPreamble =
               _httpsuccess.ToString()
               + _headerContentTypeJson.ToString()
               + _headerServer.ToString();

        public Task Default(PipeStream stream, HttpToken token, ISession session)
        {
            stream.Write(_defaultPreamble.Data, 0, _defaultPreamble.Length);
            token.ContentLength = stream.Allocate(HttpHandler._LengthSize);
            GMTDate.Default.Write(stream);
            token.ContentPostion = stream.CacheLength;
            stream.Write("<b> beetlex server</b><hr/>");
            stream.Write("path not found!");
            OnCompleted(stream, session, token);
            return Task.CompletedTask;
        }
    }
}
