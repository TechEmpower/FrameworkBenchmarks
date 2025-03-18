
using BeetleX.Light.Memory;
using System;
using System.Collections.Generic;
using System.Data;
using System.Text;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {

        private static ReadOnlySpan<byte> _plaintextPreamble =>
       "HTTP/1.1 200 OK\r\n"u8 +
       "Server: B\r\n"u8 +
       "Content-Type: text/plain\r\n"u8 +
       "Content-Length: 13\r\n"u8;

        private static ReadOnlySpan<byte> _plainTextBody => "Hello, World!"u8;
        public void Plaintext(IStreamWriter stream)
        {
            Span<byte> data = stream.WriteSequenceNetStream.GetWriteSpan(256);
            var timedata = GMTDate.Default.DATE;
            _plaintextPreamble.CopyTo(data);
            data = data.Slice(_plaintextPreamble.Length);
            timedata.Span.CopyTo(data);
            data = data.Slice(timedata.Length);
            _plainTextBody.CopyTo(data);
            //stream.Write(_plaintextPreamble.AsSpan());
            //GMTDate.Default.Write(stream);
            //stream.Write(_result_plaintext.Data.AsSpan());
            stream.WriteSequenceNetStream.WriteAdvance(_plaintextPreamble.Length + timedata.Length + _plainTextBody.Length);
        }
    }
}
