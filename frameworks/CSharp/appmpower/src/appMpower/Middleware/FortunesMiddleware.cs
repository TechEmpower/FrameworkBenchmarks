using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Primitives;

namespace appMpower; 

public class FortunesMiddleware
{
    private readonly static KeyValuePair<string, StringValues> _headerServer =
         new KeyValuePair<string, StringValues>("Server", new StringValues("k"));
    private readonly static KeyValuePair<string, StringValues> _headerContentType =
         new KeyValuePair<string, StringValues>("Content-Type", new StringValues("text/html; charset=UTF-8"));

    private readonly RequestDelegate _next;

    public FortunesMiddleware(RequestDelegate next)
    {
        _next = next;
    }

    public unsafe Task Invoke(HttpContext httpContext)
    {
        if (httpContext.Request.Path.StartsWithSegments("/fortunes", StringComparison.Ordinal))
        {
            var response = httpContext.Response; 
            response.Headers.Add(_headerServer);
            response.Headers.Add(_headerContentType);

            int payloadLength;
            IntPtr handlePointer; 

            IntPtr bytePointer = NativeMethods.Fortunes(out payloadLength, out handlePointer);
            byte[] json = new byte[payloadLength];
            Marshal.Copy(bytePointer, json, 0, payloadLength);
            NativeMethods.FreeHandlePointer(handlePointer);

            response.Headers.Add(
                new KeyValuePair<string, StringValues>("Content-Length", payloadLength.ToString()));

            return response.Body.WriteAsync(json, 0, payloadLength);
        }

        return _next(httpContext);
    }
}

public static class FortunesMiddlewareExtensions
{
    public static IApplicationBuilder UseFortunes(this IApplicationBuilder builder)
    {
        return builder.UseMiddleware<FortunesMiddleware>();
    }
}
