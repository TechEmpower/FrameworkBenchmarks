// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System.Linq;
using System.Runtime;
using System.Threading.Tasks;
using Benchmarks.Configuration;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Hosting.Server.Features;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Http.Features;
using Microsoft.Extensions.PlatformAbstractions;

namespace Benchmarks.Middleware
{
    public class DebugInfoPageMiddleware
    {
#if DEBUG
        private static readonly string _configurationName = "Debug";
#elif RELEASE
        private static readonly string _configurationName = "Release";
#else
        private static readonly string _configurationName = "";
#endif

        private readonly IHostingEnvironment _hostingEnv;
        private readonly RequestDelegate _next;
        private readonly Scenarios _scenarios;
        private readonly IServerAddressesFeature _serverAddresses;

        public DebugInfoPageMiddleware(RequestDelegate next, IServerAddressesFeature serverAddresses, IHostingEnvironment hostingEnv, Scenarios scenarios)
        {
            _next = next;
            _hostingEnv = hostingEnv;
            _scenarios = scenarios;
            _serverAddresses = serverAddresses;
        }

        public async Task Invoke(HttpContext httpContext)
        {
            httpContext.Response.ContentType = "text/html";

            await httpContext.Response.WriteAsync("<!DOCTYPE html><html><head><style>body{font-family:\"Segoe UI\",Arial,Helvetica,Sans-serif};h1,h2,h3{font-family:\"Segoe UI Light\"}</style></head><body>");
            await httpContext.Response.WriteAsync("<h1>ASP.NET Core Benchmarks</h1>");
            await httpContext.Response.WriteAsync("<h2>Configuration Information</h2>");
            await httpContext.Response.WriteAsync("<ul>");
            await httpContext.Response.WriteAsync($"<li>Environment: {_hostingEnv.EnvironmentName}</li>");
            await httpContext.Response.WriteAsync($"<li>Framework: {PlatformServices.Default.Application.RuntimeFramework.FullName}</li>");
            await httpContext.Response.WriteAsync($"<li>Server GC enabled: {GCSettings.IsServerGC}</li>");
            await httpContext.Response.WriteAsync($"<li>Configuration: {_configurationName}</li>");
            await httpContext.Response.WriteAsync($"<li>Server: {Program.Server}</li>");
            await httpContext.Response.WriteAsync($"<li>Server URLs: {string.Join(", ", _serverAddresses.Addresses)}</li>");
            await httpContext.Response.WriteAsync($"<li>Supports Send File: {httpContext.Features.Get<IHttpSendFileFeature>() != null}</li>");

            await httpContext.Response.WriteAsync($"<li>Server features:<ul>");
            foreach (var feature in httpContext.Features)
            {
                await httpContext.Response.WriteAsync($"<li>{feature.Key.Name}</li>");
            }
            await httpContext.Response.WriteAsync($"</ul></li>");

            await httpContext.Response.WriteAsync($"<li>Enabled scenarios:<ul>");
            var enabledScenarios = _scenarios.GetEnabled();
            var maxNameLength = enabledScenarios.Max(s => s.Name.Length);
            foreach (var scenario in enabledScenarios)
            {
                await httpContext.Response.WriteAsync($"<li>{scenario.Name}<ul>");
                foreach (var path in scenario.Paths)
                {
                    await httpContext.Response.WriteAsync($"<li><a href=\"{path}\">{path}</a></li>");
                }
                await httpContext.Response.WriteAsync($"</ul></li>");
            }
            await httpContext.Response.WriteAsync($"</ul></li>");


            await httpContext.Response.WriteAsync("</ul>");
            await httpContext.Response.WriteAsync("</body></html>");
        }
    }

    public static class DebugInfoPageMiddlewareExtensions
    {
        public static IApplicationBuilder RunDebugInfoPage(this IApplicationBuilder builder)
        {
            return builder.UseMiddleware<DebugInfoPageMiddleware>(builder.ServerFeatures.Get<IServerAddressesFeature>());
        }
    }
}
