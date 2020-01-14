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

        public async void db(PipeStream stream, HttpToken token, ISession session)
        {
            try
            {
                var data = await token.Db.LoadSingleQueryRow();
                await JsonSerializer.NonGeneric.Utf8.SerializeAsync(data, stream);
            }
            catch(Exception e_)
            {
                stream.Write(e_.Message);
            }
            OnCompleted(stream, session, token);
        }
    }
}
