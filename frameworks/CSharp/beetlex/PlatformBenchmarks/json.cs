using BeetleX;
using BeetleX.Buffers;
using SpanJson;
using System;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {
        public async Task Json(PipeStream stream, HttpToken token, ISession session)
        {
            JsonMessage jsonMessage = default(JsonMessage);
            jsonMessage.message = "Hello, World!";
            await JsonSerializer.NonGeneric.Utf8.SerializeAsync(jsonMessage, stream);
            OnCompleted(stream, session, token);
        }
    }
}
