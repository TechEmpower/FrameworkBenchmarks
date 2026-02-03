using System;
using System.Threading;
using System.Threading.Tasks;

using EffinitiveFramework.Core.Http;
using EffinitiveFramework.Core.Middleware;

namespace Benchmarks;

public class HttpComplianceMiddleware : IMiddleware
{

    public async ValueTask<HttpResponse> InvokeAsync(HttpRequest request, RequestDelegate next, CancellationToken cancellationToken)
    {
        var response = await next(request, cancellationToken);

        response.Headers.Add("Server", "Effinitive/1.1");
        response.Headers.Add("Date", DateTime.UtcNow.ToString("r"));

        return response;
    }

}
