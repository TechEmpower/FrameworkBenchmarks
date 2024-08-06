using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Primitives;

namespace appMpower; 

public class SingleQueryMiddleware
{
    private readonly static KeyValuePair<string, StringValues> _headerServer =
         new KeyValuePair<string, StringValues>("Server", new StringValues("k"));
    private readonly static KeyValuePair<string, StringValues> _headerContentType =
         new KeyValuePair<string, StringValues>("Content-Type", new StringValues("application/json"));

    private readonly RequestDelegate _nextStage;

    public SingleQueryMiddleware(RequestDelegate nextStage)
    {
        _nextStage = nextStage;
    }

    public unsafe Task Invoke(HttpContext httpContext)
    {
        if (httpContext.Request.Path.StartsWithSegments("/db", StringComparison.Ordinal))
        //if (httpContext.Request.Path.Value.StartsWith("/d"))
        {
            var response = httpContext.Response; 
            response.Headers.Add(_headerServer);
            response.Headers.Add(_headerContentType);

            int payloadLength;
            IntPtr handlePointer; 

            IntPtr bytePointer = NativeMethods.Db(out payloadLength, out handlePointer);
            byte[] json = new byte[payloadLength];
            Marshal.Copy(bytePointer, json, 0, payloadLength);
            NativeMethods.FreeHandlePointer(handlePointer);

            /*
            for (int i = 0; i < payloadLength; i++)
            {
                json[i] = bytePointer[i];
            }
            */

            //var json = Orm.DotnetMethods.Db();
            //int payloadLength = json.Length; 

            response.Headers.Add(
                new KeyValuePair<string, StringValues>("Content-Length", payloadLength.ToString()));

            return response.Body.WriteAsync(json, 0, payloadLength);
        }

        return _nextStage(httpContext);
    }
}

public static class SingleQueryMiddlewareExtensions
{
    public static IApplicationBuilder UseSingleQuery(this IApplicationBuilder builder)
    {
        return builder.UseMiddleware<SingleQueryMiddleware>();
    }
}