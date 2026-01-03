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

    public static async Task Invoke(HttpContext httpContext)
    {
        var response = httpContext.Response; 
        response.Headers.Add(_headerServer);
        response.Headers.Add(_headerContentType);

#if AOTDLL
        int payloadLength;
        IntPtr handlePointer; 

        IntPtr bytePointer = NativeMethods.Db(out payloadLength, out handlePointer);
        byte[] json = new byte[payloadLength];
        //Marshal.Copy(bytePointer, json, 0, payloadLength);

        unsafe
        {
            fixed (byte* dest = json)
            {
                Buffer.MemoryCopy((void*)bytePointer, dest, payloadLength, payloadLength);
            }
        }

        NativeMethods.FreeHandlePointer(handlePointer);
#else        
        byte[] json = await DotnetMethods.Db();
#endif

        response.Headers.Add(
            new KeyValuePair<string, StringValues>("Content-Length", json.Length.ToString()));

        await response.Body.WriteAsync(json);
    }
}