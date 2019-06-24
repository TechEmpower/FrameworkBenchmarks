using BeetleX;
using BeetleX.Buffers;
using SpanJson;
using System;
using System.Collections.Generic;
using System.Text;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {
        public void Json(ReadOnlySpan<byte> url, PipeStream stream, HttpToken token, ISession session)
        {
            JsonMessage jsonMessage = default(JsonMessage);
            jsonMessage.message = "Hello, World!";
            JsonSerializer.NonGeneric.Utf8.SerializeAsync(jsonMessage, stream);
            OnCompleted(stream, session, token);
        }
    }
}
