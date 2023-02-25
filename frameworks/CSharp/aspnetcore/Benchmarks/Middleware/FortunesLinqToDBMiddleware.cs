using Benchmarks.Configuration;
using Benchmarks.Data;
using System.Text.Encodings.Web;

namespace Benchmarks.Middleware;

public sealed class FortunesLinqToDBMiddleware
{
    private static readonly PathString _path = new(Scenarios.GetPath(s => s.DbFortunesLinqToDB));

    private readonly RequestDelegate _next;
    private readonly HtmlEncoder _htmlEncoder;

    public FortunesLinqToDBMiddleware(RequestDelegate next, HtmlEncoder htmlEncoder)
    {
        _next = next;
        _htmlEncoder = htmlEncoder;
    }

    public async Task Invoke(HttpContext httpContext)
    {
        if (httpContext.Request.Path.StartsWithSegments(_path, StringComparison.Ordinal))
        {
            var db = httpContext.RequestServices.GetService<LinqToDBDb>();
            var rows = await db.LoadFortunesRows();

            await MiddlewareHelpers.RenderFortunesHtml(rows, httpContext, _htmlEncoder);

            return;
        }

        await _next(httpContext);
    }
}

public static class FortunesLinq2DBMiddlewareExtensions
{
    public static IApplicationBuilder UseFortunesLinqToDB(this IApplicationBuilder builder)
    {
        return builder.UseMiddleware<FortunesLinqToDBMiddleware>();
    }
}
