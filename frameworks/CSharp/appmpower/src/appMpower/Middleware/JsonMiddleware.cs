using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Primitives;
using appMpowerAot;

public class JsonMiddleware
{
    private readonly static KeyValuePair<string, StringValues> _headerServer =
         new KeyValuePair<string, StringValues>("Server", new StringValues("k"));
    private readonly static KeyValuePair<string, StringValues> _headerContentType =
         new KeyValuePair<string, StringValues>("Content-Type", new StringValues("application/json"));
         
    private readonly RequestDelegate _nextStage;

    public JsonMiddleware(RequestDelegate nextStage)
    {
        _nextStage = nextStage;
    }

    public unsafe Task Invoke(HttpContext httpContext)
    {
        if (httpContext.Request.Path.Value.StartsWith("/j"))
        {
            var response = httpContext.Response; 
            response.Headers.Add(_headerServer);
            response.Headers.Add(_headerContentType);

            int payloadLength;

            IntPtr bytePointer = NativeMethods.JsonMessage(out payloadLength);
            //var bytePointer = NativeMethods.JsonMessage(out payloadLength);

            byte[] jsonMessage = new byte[payloadLength];
            
            Marshal.Copy(bytePointer, jsonMessage, 0, payloadLength);

            /*
            for (int i = 0; i < payloadLength; i++)
            {
                jsonMessage[i] = bytePointer[i];
            }
            */
            
            //var jsonMessage = DotnetMethods.JsonMessage();
            //int payloadLength = jsonMessage.Length; 

            response.Headers.Add(
                new KeyValuePair<string, StringValues>("Content-Length", payloadLength.ToString()));
                
            return response.Body.WriteAsync(jsonMessage, 0, payloadLength);
        }

        return _nextStage(httpContext);
    }
}

public static class JsonMiddlewareExtensions
{
    public static IApplicationBuilder UseJson(this IApplicationBuilder builder)
    {
        return builder.UseMiddleware<JsonMiddleware>();
    }
}