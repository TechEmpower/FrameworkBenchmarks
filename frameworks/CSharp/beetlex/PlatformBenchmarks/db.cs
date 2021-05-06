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

        public async ValueTask db(PipeStream stream, HttpToken token, ISession session)
        {
            try
            {
                var data = await token.Db.LoadSingleQueryRow();

                System.Text.Json.JsonSerializer.Serialize<World>(GetUtf8JsonWriter(stream, token), data, SerializerOptions);
            }
            catch (Exception e_)
            {
                HttpServer.ApiServer.Log(BeetleX.EventArgs.LogType.Error, null, $"db error {e_.Message}@{e_.StackTrace}");
                stream.Write(e_.Message);
            }
            OnCompleted(stream, session, token);
        }
    }
}
