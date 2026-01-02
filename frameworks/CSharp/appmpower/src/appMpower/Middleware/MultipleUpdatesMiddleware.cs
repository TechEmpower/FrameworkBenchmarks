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

public class MultipleUpdatesMiddelware
{
    private readonly static KeyValuePair<string, StringValues> _headerServer =
         new("Server", new StringValues("k"));
    private readonly static KeyValuePair<string, StringValues> _headerContentType =
         new("Content-Type", new StringValues("application/json"));

#if AOTDLL
   public static unsafe Task Invoke(HttpContext httpContext)
   {
      var queryString = httpContext.Request.QueryString.ToString(); 
      int count; 
      Int32.TryParse(queryString.Substring(queryString.LastIndexOf("=") + 1), out count); 
      count = count > 500 ? 500 : (count > 0 ? count : 1);

      var response = httpContext.Response;
      response.Headers.Add(_headerServer);
      response.Headers.Add(_headerContentType);

      int payloadLength;
      IntPtr handlePointer;

      IntPtr bytePointer = NativeMethods.Updates(count, out payloadLength, out handlePointer);
      byte[] json = new byte[payloadLength];
      //Marshal.Copy(bytePointer, json, 0, payloadLength);

      fixed (byte* dest = json)
      {
            Buffer.MemoryCopy((void*)bytePointer, dest, payloadLength, payloadLength);
      }

      NativeMethods.FreeHandlePointer(handlePointer);

      response.Headers.Add(
            new KeyValuePair<string, StringValues>("Content-Length", payloadLength.ToString()));

      return response.Body.WriteAsync(json, 0, payloadLength);
   }
#else
   public static async Task Invoke(HttpContext httpContext)
   {
      var queryString = httpContext.Request.QueryString.ToString(); 
      int count; 
      Int32.TryParse(queryString.Substring(queryString.LastIndexOf("=") + 1), out count); 
      count = count > 500 ? 500 : (count > 0 ? count : 1);

      var response = httpContext.Response;
      response.Headers.Add(_headerServer);
      response.Headers.Add(_headerContentType);

      byte[] json = DotnetMethods.Updates(count);

      response.Headers.Add(
            new KeyValuePair<string, StringValues>("Content-Length", json.Length.ToString()));

      await response.Body.WriteAsync(json);
   }
#endif
}