using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Primitives;

public class MultipleQueriesMiddleware
{
    private readonly static KeyValuePair<string, StringValues> _headerServer =
         new KeyValuePair<string, StringValues>("Server", new StringValues("k"));
    private readonly static KeyValuePair<string, StringValues> _headerContentType =
         new KeyValuePair<string, StringValues>("Content-Type", new StringValues("application/json"));

   private readonly RequestDelegate _next;

   public MultipleQueriesMiddleware(RequestDelegate next)
   {
      _next = next;
   }

    public unsafe Task Invoke(HttpContext httpContext)
   {
      if (httpContext.Request.Path.StartsWithSegments("/queries", StringComparison.Ordinal))
      {
         int queries = Convert.ToInt16(httpContext.Request.Query["queries"]);

         queries = queries > 500 ? 500 : (queries > 0 ? queries : 1);

         var response = httpContext.Response;
         response.Headers.Add(_headerServer);
         response.Headers.Add(_headerContentType);

         int payloadLength;
         IntPtr handlePointer;

         IntPtr bytePointer = NativeMethods.Query(queries, out payloadLength, out handlePointer);
         byte[] json = new byte[payloadLength];
         Marshal.Copy(bytePointer, json, 0, payloadLength);
         NativeMethods.FreeHandlePointer(handlePointer);

         /*
         for (int i = 0; i < payloadLength; i++)
         {
             json[i] = bytePointer[i];
         }
         */

         //var json = Orm.DotnetMethods.Query();
         //int payloadLength = json.Length; 

         response.Headers.Add(
             new KeyValuePair<string, StringValues>("Content-Length", payloadLength.ToString()));

         return response.Body.WriteAsync(json, 0, payloadLength);
      }

      return _next(httpContext);
   }
}

public static class MultipleQueriesMiddlewareExtensions
{
   public static IApplicationBuilder UseMultipleQueries(this IApplicationBuilder builder)
   {
      return builder.UseMiddleware<MultipleQueriesMiddleware>();
   }
}
