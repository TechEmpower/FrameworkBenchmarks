using System;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;

public class PlaintextMiddleware
{
    //private static readonly byte[] HelloWorldPayload = Encoding.UTF8.GetBytes("Hello, World!");
    private readonly RequestDelegate _nextStage;

    public PlaintextMiddleware(RequestDelegate nextStage)
    {
        _nextStage = nextStage;
    }

    public Task Invoke(HttpContext httpContext)
    {
        if (httpContext.Request.Path.Value.StartsWith("/p"))
        {
            return WriteResponse(httpContext.Response);
        }

        return _nextStage(httpContext);
    }

    public static unsafe Task WriteResponse(HttpResponse response)
    {
        var helloWorldPointer = NativeMethods.HelloWorld();
        string helloWorld = new string(helloWorldPointer);
        //Console.WriteLine("here i am");
        //Console.WriteLine(helloWorld);
        //Console.WriteLine("again");

        byte[] HelloWorldPayload = Encoding.UTF8.GetBytes(helloWorld);
        var payloadLength = HelloWorldPayload.Length;
        response.StatusCode = 200;
        response.ContentType = "text/plain";
        response.ContentLength = payloadLength;
        return response.Body.WriteAsync(HelloWorldPayload, 0, payloadLength);
    }
}

public static class PlaintextMiddlewareExtensions
{
    public static IApplicationBuilder UsePlainText(this IApplicationBuilder builder)
    {
        return builder.UseMiddleware<PlaintextMiddleware>();
    }
}
