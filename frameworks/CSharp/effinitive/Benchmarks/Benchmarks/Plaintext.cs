using System.Threading;
using System.Threading.Tasks;

using EffinitiveFramework.Core;

namespace Benchmarks.Benchmarks;

public class Plaintext : NoRequestEndpointBase<string>
{
    protected override string Method => "GET";

    protected override string Route => "/plaintext";

    public override ValueTask<string> HandleAsync(CancellationToken cancellationToken = default)
    {
        return ValueTask.FromResult("Hello, World!");
    }

}
