using System;
using System.Text;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using appMpowerAot;

public class SingleQueryRawMiddleware
{
    private readonly RequestDelegate _nextStage;

    public SingleQueryRawMiddleware(RequestDelegate nextStage)
    {
        _nextStage = nextStage;
    }

    public unsafe Task Invoke(HttpContext httpContext)
    {
        if (httpContext.Request.Path.StartsWithSegments("/db", StringComparison.Ordinal))
        {
            var response = httpContext.Response; 
            response.StatusCode = 200;
            response.ContentType = "application/json";

            var bytePointer = NativeMethods.Db();
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

            //var jsonMessage = DotnetMethods.Db();
            //int payloadLength = jsonMessage.Length; 

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