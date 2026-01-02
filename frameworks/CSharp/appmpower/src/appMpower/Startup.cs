using System;
using System.Text.Encodings.Web;
using System.Text.Unicode;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;

namespace appMpower;

public sealed class Startup
{
    public void ConfigureServices(IServiceCollection services, IConfiguration config)
    {
        var settings = new TextEncoderSettings(UnicodeRanges.BasicLatin, UnicodeRanges.Katakana, UnicodeRanges.Hiragana);

        settings.AllowCharacter('—');
        services.AddWebEncoders(options =>
        {
            options.TextEncoderSettings = settings;
        });
    }

    public void Configure(WebApplication app)
    {
        // Build all handlers ONCE. These instances live for the lifetime of the app.
        // Note: their _nextStage should be the branch's next (usually 404 here).
        RequestDelegate notFound = ctx =>
        {
            ctx.Response.StatusCode = StatusCodes.Status404NotFound;
            return ctx.Response.WriteAsync("Not found");
        };

        var plaintext = new PlaintextMiddleware(notFound);
        var json = new JsonMiddleware(notFound);
        var db = new SingleQueryMiddleware(notFound);
        var multipleQueries = new MultipleQueriesMiddleware(notFound);
        var multipleUpdates = new MultipleUpdatesMiddelware(notFound);
        var caching = new CachingMiddleware(notFound);
        var fortunes = new FortunesMiddleware(notFound);

        app.Use(async (httpContext, next) =>
        {
            // ---------- PLAINTEXT ----------
            if (httpContext.Request.Path.StartsWithSegments("/plaintext", StringComparison.Ordinal))
            {
                await plaintext.Invoke(httpContext);
                return;
            }

            // ---------- JSON ----------
            if (httpContext.Request.Path.StartsWithSegments("/json", StringComparison.Ordinal))
            {
                await json.Invoke(httpContext);
                return;
            }

            // ---------- SINGLE QUERY ----------
            if (httpContext.Request.Path.StartsWithSegments("/db", StringComparison.Ordinal))
            {
                await db.Invoke(httpContext);
                return;
            }

            // ---------- CACHING ----------
            if (httpContext.Request.Path.StartsWithSegments("/cached-worlds", StringComparison.Ordinal))
            {
                await caching.Invoke(httpContext);
                return;
            }

            // ---------- CACHING ----------
            if (httpContext.Request.Path.StartsWithSegments("/cached-worlds", StringComparison.Ordinal))
            {
                await caching.Invoke(httpContext);
                return;
            }

            // ---------- FORTUNES ----------
            if (httpContext.Request.Path.StartsWithSegments("/fortunes", StringComparison.Ordinal))
            {
                await fortunes.Invoke(httpContext);
                return;
            }

            // ---------- MULTIPLE QUERIES ----------
            if (httpContext.Request.Path.StartsWithSegments("/queries", StringComparison.Ordinal))
            {
                await multipleQueries.Invoke(httpContext);
                return;
            }

            // ---------- MULTIPLE UPDATES ----------
            if (httpContext.Request.Path.StartsWithSegments("/updates", StringComparison.Ordinal))
            {
                await multipleUpdates.Invoke(httpContext);
                return;
            }

            // If not handled here, let the rest of the pipeline run (404/endpoints/etc)
            await next();
        });

        // Optional: fallthrough response if nothing else handles it
        app.Run(ctx =>
        {
            ctx.Response.StatusCode = StatusCodes.Status404NotFound;
            return ctx.Response.WriteAsync("Not found");
        });
    }
}