using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Primitives;
using appMpowerAot;

public class SingleQueryRawMiddleware
{
    private readonly static KeyValuePair<string, StringValues> _headerServer =
         new KeyValuePair<string, StringValues>("Server", new StringValues("k"));

    private readonly RequestDelegate _nextStage;

    public SingleQueryRawMiddleware(RequestDelegate nextStage)
    {
        _nextStage = nextStage;
    }

    public unsafe Task Invoke(HttpContext httpContext)
    {
        if (httpContext.Request.Path.Value.StartsWith("/d"))
        {
            var response = httpContext.Response; 
            response.Headers.Add(_headerServer);
            response.ContentType = "application/json";

            int payloadLength;

            IntPtr bytePointer = NativeMethods.JsonMessage(out payloadLength);
            //var bytePointer = NativeMethods.Db(out payloadLength);

            byte[] jsonMessage = new byte[payloadLength];
            Marshal.Copy(bytePointer, jsonMessage, 0, payloadLength);

            /*
            for (int i = 0; i < payloadLength; i++)
            {
                jsonMessage[i] = bytePointer[i];
            }
            */

            //var jsonMessage = DotnetMethods.Db();
            //int payloadLength = jsonMessage.Length; 

            response.ContentLength = payloadLength; 
            return response.Body.WriteAsync(jsonMessage, 0, payloadLength);
        }

        return _nextStage(httpContext);
    }
}

public static class SingleQueryRawMiddlewareExtensions
{
    public static IApplicationBuilder UseSingleQueryRaw(this IApplicationBuilder builder)
    {
        return builder.UseMiddleware<SingleQueryRawMiddleware>();
    }
}