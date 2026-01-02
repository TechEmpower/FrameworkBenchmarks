using System.Collections.Generic;
using System.Text.Json; 
using System.Threading.Tasks;
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
         new("Server", new StringValues("k"));
    private readonly static KeyValuePair<string, StringValues> _headerContentType =
         new("Content-Type", new StringValues("application/json"));
         
    public static Task Invoke(HttpContext httpContext)
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
}