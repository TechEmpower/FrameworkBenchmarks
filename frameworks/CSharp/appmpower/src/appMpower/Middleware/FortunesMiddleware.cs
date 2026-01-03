using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text.Encodings.Web;
using System.Text.Unicode;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Primitives;

#if !AOTDLL
using appMpower.Orm;
#endif

namespace appMpower; 

public class FortunesMiddleware
{
    private readonly static KeyValuePair<string, StringValues> _headerServer =
         new("Server", new StringValues("k"));
    private readonly static KeyValuePair<string, StringValues> _headerContentType =
         new("Content-Type", new StringValues("text/html; charset=UTF-8"));

    private static readonly byte[] _delimiter = [0xFF, 0xFF, 0xFF, 0xFF];

    public static async Task Invoke(HttpContext httpContext)
    {
        var response = httpContext.Response; 
        response.Headers.Add(_headerServer);
        response.Headers.Add(_headerContentType);

#if AOTDLL
        int payloadLength;
        IntPtr handlePointer; 

        IntPtr bytePointer = NativeMethods.Fortunes(out payloadLength, out handlePointer);
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
        byte[] json = await DotnetMethods.Fortunes();
#endif
        response.Headers.Add(
            new KeyValuePair<string, StringValues>("Content-Length", json.Length.ToString()));

        await response.Body.WriteAsync(json);
    }
}