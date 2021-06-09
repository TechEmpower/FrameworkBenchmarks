using System.Collections.Generic;
using System.IO.Pipelines;
using System.Web;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Primitives;
using PlatformBenchmarks;

namespace appMpower
{
   public static class FortunesView
   {
      private readonly static KeyValuePair<string, StringValues> _headerServer =
         new KeyValuePair<string, StringValues>("Server", "k");
      private readonly static KeyValuePair<string, StringValues> _headerContentType =
         new KeyValuePair<string, StringValues>("Content-Type", "text/html; charset=UTF-8");

      private readonly static AsciiString _fortunesTableStart = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";
      private readonly static AsciiString _fortunesRowStart = "<tr><td>";
      private readonly static AsciiString _fortunesColumn = "</td><td>";
      private readonly static AsciiString _fortunesRowEnd = "</td></tr>";
      private readonly static AsciiString _fortunesTableEnd = "</table></body></html>";

      public static void Render(IHeaderDictionary headerDictionary, PipeWriter pipeWriter, List<Fortune> fortunes)
      {
         headerDictionary.Add(_headerServer);
         headerDictionary.Add(_headerContentType);

         var bufferWriter = new BufferWriter<WriterAdapter>(new WriterAdapter(pipeWriter), 1600);

         bufferWriter.Write(_fortunesTableStart);

         foreach (var fortune in fortunes)
         {
            bufferWriter.Write(_fortunesRowStart);
            bufferWriter.WriteNumeric((uint)fortune.Id);
            bufferWriter.Write(_fortunesColumn);
            bufferWriter.WriteUtf8String(HttpUtility.HtmlEncode(fortune.Message));
            bufferWriter.Write(_fortunesRowEnd);
         }

         bufferWriter.Write(_fortunesTableEnd);
         headerDictionary.Add(new KeyValuePair<string, StringValues>("Content-Length", bufferWriter.Buffered.ToString()));
         bufferWriter.Commit();
      }
   }
}