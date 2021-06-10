using System.Collections.Generic;
using System.IO.Pipelines;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Primitives;
using PlatformBenchmarks;

namespace appMpower.Kestrel
{
   public static class PlainText
   {
      private readonly static KeyValuePair<string, StringValues> _headerServer =
         new KeyValuePair<string, StringValues>("Server", new StringValues("k"));
      private readonly static KeyValuePair<string, StringValues> _headerContentType =
         new KeyValuePair<string, StringValues>("Content-Type", new StringValues("text/plain"));

      public static void Render(IHeaderDictionary headerDictionary, PipeWriter pipeWriter, AsciiString utf8String)
      {
         headerDictionary.Add(_headerServer);
         headerDictionary.Add(_headerContentType);
         headerDictionary.Add(new KeyValuePair<string, StringValues>("Content-Length", utf8String.Length.ToString()));

         var bufferWriter = new BufferWriter<WriterAdapter>(new WriterAdapter(pipeWriter), 208);
         bufferWriter.Write(utf8String);
         bufferWriter.Commit();
      }
   }
}