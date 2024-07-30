using System;
using System.Text;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using appMpowerAot;

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
            var response = httpContext.Response; 
            response.StatusCode = 200;
            response.ContentType = "application/json";

            //var jsonMessage = Encoding.UTF8.GetBytes(new string(NativeMethods.JsonMessage()));
            //int payloadLength = jsonMessage.Length; 

            /*
            int currentThreadId = Thread.CurrentThread.ManagedThreadId; 
            int payloadLength = NativeMethods.JsonMessage31(currentThreadId);
            byte* bytePointer = NativeMethods.JsonMessage32(currentThreadId);
            */

            var bytePointer = NativeMethods.JsonMessage();
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
            
            //var jsonMessage = DotnetMethods.JsonMessage();
            //int payloadLength = jsonMessage.Length; 

            response.ContentLength = payloadLength; 
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