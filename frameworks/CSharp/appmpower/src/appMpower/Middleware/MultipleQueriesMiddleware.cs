using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Primitives;

#if !AOTDLL
using appMpower.Orm;
#endif

namespace appMpower; 

public class MultipleQueriesMiddleware
{
   private readonly static KeyValuePair<string, StringValues> _headerServer =
         new("Server", new StringValues("k"));
   private readonly static KeyValuePair<string, StringValues> _headerContentType =
         new("Content-Type", new StringValues("application/json"));

   public static async Task Invoke(HttpContext httpContext)
   {
      var queryString = httpContext.Request.QueryString.ToString(); 
      int queries; 
      Int32.TryParse(queryString.Substring(queryString.LastIndexOf("=") + 1), out queries); 
      queries = queries > 500 ? 500 : (queries > 0 ? queries : 1);

      var response = httpContext.Response;
      response.Headers.Add(_headerServer);
      response.Headers.Add(_headerContentType);

#if AOTDLL
      int payloadLength;
      IntPtr handlePointer;

      IntPtr bytePointer = NativeMethods.Query(queries, out payloadLength, out handlePointer);
      byte[] json = new byte[payloadLength];
      //Marshal.Copy(bytePointer, json, 0, payloadLength);

      fixed (byte* dest = json)
      {
            Buffer.MemoryCopy((void*)bytePointer, dest, payloadLength, payloadLength);
      }

      NativeMethods.FreeHandlePointer(handlePointer);
#else
      byte[] json = await DotnetMethods.Query(queries);
#endif
      response.Headers.Add(
            new KeyValuePair<string, StringValues>("Content-Length", json.Length.ToString()));

      await response.Body.WriteAsync(json);
   }
}