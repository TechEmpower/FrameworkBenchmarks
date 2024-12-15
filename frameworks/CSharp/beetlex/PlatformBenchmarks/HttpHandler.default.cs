
using BeetleX.Light.Memory;
using System;
using System.Buffers;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {
        private readonly static AsciiString _defaultPreamble =
               _httpsuccess.ToString()
               + _headerContentTypeJson.ToString()
               + _headerServer.ToString();

        public ValueTask Default(IStreamWriter stream)
        { 
            
            stream.Write(_defaultPreamble.Data, 0, _defaultPreamble.Length);
            ContentLengthMemory contentLength = new ContentLengthMemory();
            contentLength.Data = GetContentLengthMemory(stream);
            GMTDate.Default.Write(stream);
            stream.WriteSequenceNetStream.StartWriteLength();
            stream.WriteString("<b> beetlex server</b><hr/>");
            stream.WriteString("path not found!");
            var length = stream.WriteSequenceNetStream.EndWriteLength();
            contentLength.Full(length);
            return ValueTask.CompletedTask;
        }
    }
}
