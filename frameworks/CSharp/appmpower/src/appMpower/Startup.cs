using System;
using System.Text.Encodings.Web;
using System.Text.Unicode;
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
        app.Use(async (httpContext, next) =>
        {
            // ---------- PLAINTEXT ----------
            if (httpContext.Request.Path.StartsWithSegments("/plaintext", StringComparison.Ordinal))
            {
                await PlaintextMiddleware.Invoke(httpContext);
                return;
            }

            // ---------- JSON ----------
            if (httpContext.Request.Path.StartsWithSegments("/json", StringComparison.Ordinal))
            {
                await JsonMiddleware.Invoke(httpContext);
                return;
            }

            // ---------- SINGLE QUERY ----------
            if (httpContext.Request.Path.StartsWithSegments("/db", StringComparison.Ordinal))
            {
                await SingleQueryMiddleware.Invoke(httpContext);
                return;
            }

            // ---------- CACHING ----------
            if (httpContext.Request.Path.StartsWithSegments("/cached-worlds", StringComparison.Ordinal))
            {
                await CachingMiddleware.Invoke(httpContext);
                return;
            }

            // ---------- CACHING ----------
            if (httpContext.Request.Path.StartsWithSegments("/cached-worlds", StringComparison.Ordinal))
            {
                await CachingMiddleware.Invoke(httpContext);
                return;
            }

            // ---------- FORTUNES ----------
            if (httpContext.Request.Path.StartsWithSegments("/fortunes", StringComparison.Ordinal))
            {
                await FortunesMiddleware.Invoke(httpContext);
                return;
            }

            // ---------- MULTIPLE QUERIES ----------
            if (httpContext.Request.Path.StartsWithSegments("/queries", StringComparison.Ordinal))
            {
                await MultipleQueriesMiddleware.Invoke(httpContext);
                return;
            }

            // ---------- MULTIPLE UPDATES ----------
            if (httpContext.Request.Path.StartsWithSegments("/updates", StringComparison.Ordinal))
            {
                await MultipleQueriesMiddleware.Invoke(httpContext);
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