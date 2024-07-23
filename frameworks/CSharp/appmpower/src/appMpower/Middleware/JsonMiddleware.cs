using System;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;

public class JsonMiddleware
{
    private const int BufferSize = 27;
    private readonly RequestDelegate _nextStage;

    public JsonMiddleware(RequestDelegate nextStage)
    {
        _nextStage = nextStage;
    }

    public unsafe Task Invoke(HttpContext httpContext)
    {
        if (httpContext.Request.Path.StartsWithSegments("/json", StringComparison.Ordinal))
        {
            httpContext.Response.StatusCode = 200;
            httpContext.Response.ContentType = "application/json";

            //var jsonMessage = Encoding.UTF8.GetBytes(new string(NativeMethods.JsonMessage()));

            /*
            int currentThreadId = Thread.CurrentThread.ManagedThreadId; 
            int payloadLength = NativeMethods.JsonMessage31(currentThreadId);
            byte* bytePointer = NativeMethods.JsonMessage32(currentThreadId);
            */


            var bytePointer = NativeMethods.JsonMessage2();
            int payloadLength = 0;

            while (bytePointer[payloadLength] != 0)
            {
                payloadLength++;
            }

            byte[] jsonMessage = new byte[payloadLength];

            for (int i = 0; i < payloadLength; i++)
            {
                jsonMessage[i] = bytePointer[i];
            }
            
            //NativeMethods.JsonMessage33(currentThreadId);

            httpContext.Response.ContentLength = payloadLength; 
            return httpContext.Response.Body.WriteAsync(jsonMessage, 0, payloadLength);
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