using BeetleX;
using BeetleX.Buffers;
using SpanJson;
using System;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public partial class  HttpHandler
    {
        public async ValueTask queries(string queryString, PipeStream stream, HttpToken token, ISession session)
        {
            int count = 1;
            if(!string.IsNullOrEmpty(queryString))
            {
                var values = queryString.Split('=');
                if(values.Length>1)
                {
                    if(int.TryParse(values[1],out int size))
                    {
                        count = size;
                    }
                }
            }
            if (count > 500)
                count = 500;
            if (count < 1)
                count = 1;
            try
            {
                var data = await token.Db.LoadMultipleQueriesRows(count);
                stream.Write(_jsonResultPreamble.Data, 0, _jsonResultPreamble.Length);
                token.ContentLength = stream.Allocate(HttpHandler._LengthSize);
                GMTDate.Default.Write(stream);
                token.ContentPostion = stream.CacheLength;
                System.Text.Json.JsonSerializer.Serialize(GetUtf8JsonWriter(stream, token), data, SerializerOptions);
            }
            catch (Exception e_)
            {
                stream.Write(e_.Message);
            }
            OnCompleted(stream, session, token);
        }
    }
}
