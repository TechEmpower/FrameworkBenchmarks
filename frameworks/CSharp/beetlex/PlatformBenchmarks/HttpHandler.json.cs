using BeetleX;

using BeetleX.Light.Memory;

using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.Json;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {

        private static ReadOnlySpan<byte> _jsonPreamble =>
     "HTTP/1.1 200 OK\r\n"u8 +
     "Server: B\r\n"u8 +
     "Content-Type: application/json\r\n"u8 +
     "Content-Length: 27\r\n"u8;
        public void Json(IStreamWriter stream)
        {
            stream.Write(_jsonPreamble);
            GMTDate.Default.Write(stream);
            var jsonWriter = GetJsonWriter(stream);
            using (var unflush = stream.UnFlush())
            {
                jsonWriter.WriteStartObject();
                jsonWriter.WriteString("message", "Hello, World!");
                jsonWriter.WriteEndObject();
                jsonWriter.Flush();
            }

        }
    }
}
