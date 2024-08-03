using System.Collections.Generic;
using System.Globalization;
using System.Text;
using System.Threading.Tasks;
using System.Web;
using PlatformBenchmarks;
using appMpower.Orm.Objects;

namespace appMpower.Orm
{
   public static class FortunesView
   {
      public static char[] _fortunesTableStart = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>".ToCharArray();
      public static char[] _fortunesRowStart = "<tr><td>".ToCharArray();
      public static char[] _fortunesColumn = "</td><td>".ToCharArray();
      public static char[] _fortunesRowEnd = "</td></tr>".ToCharArray();
      public static char[] _fortunesTableEnd = "</table></body></html>".ToCharArray();

      public static void Render(List<Fortune> fortunes)
      {
         var writer = StringBuilderCache.Acquire();

         writer.Append(_fortunesTableStart);

         foreach (var fortune in fortunes)
         {
            writer.Append(_fortunesRowStart).Append(fortune.Id.ToString(CultureInfo.InvariantCulture)).Append(_fortunesColumn).Append(HttpUtility.HtmlEncode(fortune.Message)).Append(_fortunesRowEnd);
         }

         writer.Append(_fortunesTableEnd);

         var test = Encoding.UTF8.GetBytes(StringBuilderCache.GetStringAndRelease(writer));
      }
   }
}