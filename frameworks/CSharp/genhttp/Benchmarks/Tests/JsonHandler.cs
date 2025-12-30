using Benchmarks.Utilities;

using GenHTTP.Api.Content;
using GenHTTP.Api.Protocol;

namespace Benchmarks.Tests;

public sealed class JsonResult
{

    public string Message { get; set; }
}

public sealed class JsonHandler : IHandler
{
    private static readonly FlexibleContentType ContentType = new(GenHTTP.Api.Protocol.ContentType.ApplicationJson, "utf-8");

    public ValueTask PrepareAsync() => new();

    public ValueTask<IResponse> HandleAsync(IRequest request)
    {
        var result = new JsonResult()
        {
            Message = "Hello, World!"
        };

        var response = request.Respond()
                              .Content(new FixedLengthJsonContent(result))
                              .Type(ContentType)
                              .Build();

        return new(response);
    }

}
