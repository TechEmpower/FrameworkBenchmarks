using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text.Json; 
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Primitives;
using appMpower.Serializers; 

namespace appMpower; 

public class JsonMiddleware
{
    private readonly static JsonWriterOptions _jsonWriterOptions = new JsonWriterOptions
    {
        Indented = false, 
        SkipValidation = true
    };

    private readonly static JsonMessageSerializer _jsonMessageSerializer = new JsonMessageSerializer();

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

            using var utf8JsonWriter = new Utf8JsonWriter(httpContext.Response.Body, _jsonWriterOptions);

            _jsonMessageSerializer.Serialize(utf8JsonWriter, new JsonMessage { Message = "Hello, World!" });

            response.Headers.Add(
                new KeyValuePair<string, StringValues>("Content-Length", utf8JsonWriter.BytesPending.ToString()));

            utf8JsonWriter.Flush();

            return Task.CompletedTask;
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