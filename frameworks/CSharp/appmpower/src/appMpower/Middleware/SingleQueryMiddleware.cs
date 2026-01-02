using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Threading.Tasks;
using appMpower.Orm;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Primitives;

namespace appMpower; 

public class SingleQueryMiddleware
{
    private readonly static KeyValuePair<string, StringValues> _headerServer =
         new("Server", new StringValues("k"));
    private readonly static KeyValuePair<string, StringValues> _headerContentType =
         new("Content-Type", new StringValues("application/json"));

#if AOTDLL
    public static unsafe Task Invoke(HttpContext httpContext)
    {
        var response = httpContext.Response; 
        response.Headers.Add(_headerServer);
        response.Headers.Add(_headerContentType);

        int payloadLength;
        IntPtr handlePointer; 

        IntPtr bytePointer = NativeMethods.Db(out payloadLength, out handlePointer);
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
        var response = httpContext.Response; 
        response.Headers.Add(_headerServer);
        response.Headers.Add(_headerContentType);

        byte[] json = DotnetMethods.Db();

        response.Headers.Add(
            new KeyValuePair<string, StringValues>("Content-Length", json.Length.ToString()));

        await response.Body.WriteAsync(json);
    }
#endif    
}