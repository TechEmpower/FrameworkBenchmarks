using System.Collections.Generic;
using System.IO.Pipelines;
using System.Text;
using System.Threading.Tasks;
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

      public static char[] _fortunesTableStart = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>".ToCharArray();
      public static char[] _fortunesRowStart = "<tr><td>".ToCharArray();
      public static char[] _fortunesColumn = "</td><td>".ToCharArray();
      public static char[] _fortunesRowEnd = "</td></tr>".ToCharArray();
      public static char[] _fortunesTableEnd = "</table></body></html>".ToCharArray();

      public static async Task Render(IHeaderDictionary headerDictionary, PipeWriter pipeWriter, List<Fortune> fortunes)
      {
         headerDictionary.Add(_headerServer);
         headerDictionary.Add(_headerContentType);

         var writer = StringBuilderCache.Acquire();

         writer.Append(_fortunesTableStart);

         foreach (var fortune in fortunes)
         {
            writer.Append(_fortunesRowStart).Append(fortune.Id).Append(_fortunesColumn).Append(HttpUtility.HtmlEncode(fortune.Message)).Append(_fortunesRowEnd);
         }

         writer.Append(_fortunesTableEnd);

         headerDictionary.Add(new KeyValuePair<string, StringValues>("Content-Length", (writer.Length + 32).ToString()));

         await pipeWriter.WriteAsync(Encoding.UTF8.GetBytes(StringBuilderCache.GetStringAndRelease(writer)));
         pipeWriter.Complete();
      }
   }
}