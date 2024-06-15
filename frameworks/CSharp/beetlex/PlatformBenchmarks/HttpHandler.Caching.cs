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


        public async Task caching(string queryString, IStreamWriter stream)
        {
            int count = 1;
            if (!string.IsNullOrEmpty(queryString))
            {
                var values = queryString.Split('=');
                if (values.Length > 1)
                {
                    if (int.TryParse(values[1], out int size))
                    {
                        count = size;
                    }
                }
            }
            if (count > 500)
                count = 500;
            if (count < 1)
                count = 1;
            ContentLengthMemory content = new ContentLengthMemory();
            try
            {
                var data = await _db.LoadCachedQueries(count);
                stream.Write(_jsonResultPreamble.Data, 0, _jsonResultPreamble.Length);
                content.Data = GetContentLengthMemory(stream);
                GMTDate.Default.Write(stream);
                stream.WriteSequenceNetStream.StartWriteLength();

                var jsonWriter = GetJsonWriter(stream);
                using (var unflush = stream.UnFlush())
                {
                    jsonWriter.WriteStartArray();
                    foreach (var item in data)
                    {
                        jsonWriter.WriteStartObject();
                        jsonWriter.WriteNumber("Id", item.Id);
                        jsonWriter.WriteNumber("RandomNumber", item.RandomNumber);
                        jsonWriter.WriteEndObject();
                    }
                    jsonWriter.WriteEndArray();
                    jsonWriter.Flush();

                }
            }
            catch (Exception e_)
            {
                Context.GetLoger(BeetleX.Light.Logs.LogLevel.Error)?.WriteException(Context, "PlatformBenchmarks", "caching", e_);
                stream.WriteString(e_.Message);
            }
            var len = stream.WriteSequenceNetStream.EndWriteLength();
            content.Full(len);

        }
    }
}
