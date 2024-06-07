using BeetleX.Light.Memory;
using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {


        public async Task db(IStreamWriter stream)
        {
            ContentLengthMemory content = new ContentLengthMemory();

            try
            {
                var data = await _db.LoadSingleQueryRow();
                stream.Write(_jsonResultPreamble.Data, 0, _jsonResultPreamble.Length);
                content.Data = GetContentLengthMemory(stream);
                GMTDate.Default.Write(stream);
                stream.WriteSequenceNetStream.StartWriteLength();
                var jsonWriter = GetJsonWriter(stream);
                using (var unflush = stream.UnFlush())
                {
                    jsonWriter.WriteStartObject();
                    jsonWriter.WriteNumber("Id", data.Id);
                    jsonWriter.WriteNumber("RandomNumber", data.RandomNumber);
                    jsonWriter.WriteEndObject();
                    System.Text.Json.JsonSerializer.Serialize<World>((Stream)stream.WriteSequenceNetStream, data);
                }

            }
            catch (Exception e_)
            {
                Context.GetLoger(BeetleX.Light.Logs.LogLevel.Error)?.WriteException(Context, "PlatformBenchmarks", "db", e_);
                stream.WriteString(e_.Message);
            }
            var len = stream.WriteSequenceNetStream.EndWriteLength();
            content.Full(len);

        }
    }
}
