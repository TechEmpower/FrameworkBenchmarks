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


        public void Json(IStreamWriter stream)
        {
            stream.Write(_jsonPreamble.Data, 0, _jsonPreamble.Length);
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
