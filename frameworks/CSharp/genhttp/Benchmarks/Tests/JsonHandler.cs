using System.Text.Json;

using GenHTTP.Api.Content;
using GenHTTP.Api.Protocol;

using GenHTTP.Modules.Conversion.Serializers.Json;

namespace Benchmarks.Tests;

public sealed class JsonResult
{

    public string Message { get; set; }
}

public sealed class JsonHandler : IHandler
{
    private static readonly FlexibleContentType _ContentType = new(ContentType.ApplicationJson, "utf-8");

    private static readonly JsonSerializerOptions _Options = new();

    public ValueTask PrepareAsync() => new();

    public ValueTask<IResponse> HandleAsync(IRequest request)
    {
        var result = new JsonResult()
        {
            Message = "Hello, World!"
        };

        var response = request.Respond()
                              .Content(new JsonContent(result, _Options))
                              .Type(_ContentType)
                              .Build();

        return new(response);
    }

}
