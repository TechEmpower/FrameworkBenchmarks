using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Primitives;

using appMpower.Serializers;

namespace appMpower; 

public class SingleQueryMiddleware
{
    private readonly static JsonWriterOptions _jsonWriterOptions = new()
    {
        Indented = false, 
        SkipValidation = true
    };

    //private readonly static WorldSerializer _worldSerializer = new();

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
    //public async Task Invoke(HttpContext httpContext)
    {
        //if (httpContext.Request.Path.StartsWithSegments("/db", StringComparison.Ordinal))
        {
            var response = httpContext.Response; 
            response.Headers.Add(_headerServer);
            response.Headers.Add(_headerContentType);

            int payloadLength;
            IntPtr handlePointer; 

            IntPtr bytePointer = NativeMethods.Db(out payloadLength, out handlePointer);
            byte[] json = new byte[payloadLength];
            //Marshal.Copy(bytePointer, json, 0, payloadLength);

            fixed (byte* dest = json)
            {
                Buffer.MemoryCopy((void*)bytePointer, dest, payloadLength, payloadLength);
            }

            NativeMethods.FreeHandlePointer(handlePointer);

            response.Headers.Add(
                new KeyValuePair<string, StringValues>("Content-Length", payloadLength.ToString()));

            return response.Body.WriteAsync(json, 0, payloadLength);

            /*
            var world = await RawDbNpgsql.LoadSingleQueryRow(); 
            //var world = await RawDbMySql.LoadSingleQueryRow(); 
            //var world = RawDbMySql.LoadSingleQueryRow(); 
            
            using var utf8JsonWriter = new Utf8JsonWriter(httpContext.Response.Body, _jsonWriterOptions);

            _worldSerializer.Serialize(utf8JsonWriter, world);

            await utf8JsonWriter.FlushAsync();
            */
        }

        //return _nextStage(httpContext);
        //await _nextStage(httpContext);
    }
}

public static class SingleQueryMiddlewareExtensions
{
    public static IApplicationBuilder UseSingleQuery(this IApplicationBuilder builder)
    {
        return builder.UseMiddleware<SingleQueryMiddleware>();
    }
}