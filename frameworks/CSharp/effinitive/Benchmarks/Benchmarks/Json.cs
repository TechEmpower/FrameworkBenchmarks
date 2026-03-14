using System.Threading;
using System.Threading.Tasks;

using EffinitiveFramework.Core;

namespace Benchmarks.Benchmarks;

public class JsonResult
{
    public string Message { get; set; }
}

public class Json : NoRequestEndpointBase<JsonResult>
{
    protected override string Method => "GET";

    protected override string Route => "/json";

    protected override string ContentType => "application/json; charset=utf-8";

    public override ValueTask<JsonResult> HandleAsync(CancellationToken cancellationToken = default)
    {
        return ValueTask.FromResult(new JsonResult() { Message = "Hello, World!" });
    }

}
