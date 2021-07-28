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

      public static string _fortunesTableStart = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";
      public static string _fortunesRowStart = "<tr><td>";
      public static string _fortunesColumn = "</td><td>";
      public static string _fortunesRowEnd = "</td></tr>";
      public static string _fortunesTableEnd = "</table></body></html>";

      public static async Task Render(IHeaderDictionary headerDictionary, PipeWriter pipeWriter, List<Fortune> fortunes)
      {
         headerDictionary.Add(_headerServer);
         headerDictionary.Add(_headerContentType);

         string writer = _fortunesTableStart;

         foreach (var fortune in fortunes)
         {
            writer += _fortunesRowStart + fortune.Id + _fortunesColumn + HttpUtility.HtmlEncode(fortune.Message) + _fortunesRowEnd;
         }

         writer += _fortunesTableEnd;

         headerDictionary.Add(new KeyValuePair<string, StringValues>("Content-Length", (writer.Length + 32).ToString()));

         await pipeWriter.WriteAsync(Encoding.UTF8.GetBytes(writer));
         pipeWriter.Complete();
      }
   }
}