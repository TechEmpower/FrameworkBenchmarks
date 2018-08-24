// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Data.Common;
using System.Text.Encodings.Web;
using System.Text.Unicode;
using Benchmarks.Configuration;
using Benchmarks.Data;
using Benchmarks.Middleware;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using MySql.Data.MySqlClient;
using Npgsql;

namespace Benchmarks
{
    public class Startup
    {
        public Startup(IHostingEnvironment hostingEnv, Scenarios scenarios)
        {
            // Set up configuration sources.
            var builder = new ConfigurationBuilder()
                .SetBasePath(hostingEnv.ContentRootPath)
                .AddJsonFile("appsettings.json")
                .AddJsonFile($"appsettings.{hostingEnv.EnvironmentName}.json", optional: true)
                .AddEnvironmentVariables()
                .AddCommandLine(Program.Args)
                ;

            Configuration = builder.Build();

            Scenarios = scenarios;
        }

        public IConfigurationRoot Configuration { get; set; }

        public Scenarios Scenarios { get; }

        public void ConfigureServices(IServiceCollection services)
        {
            services.Configure<AppSettings>(Configuration);

            // We re-register the Scenarios as an instance singleton here to avoid it being created again due to the
            // registration done in Program.Main
            services.AddSingleton(Scenarios);

            // Common DB services
            services.AddSingleton<ConcurrentRandom>();

            var appSettings = Configuration.Get<AppSettings>();
            Console.WriteLine($"Database: {appSettings.Database}");

            if (appSettings.Database == DatabaseServer.PostgreSql)
            {
                if (Scenarios.Any("Ef"))
                {
                    services.AddDbContextPool<ApplicationDbContext>(options => options.UseNpgsql(appSettings.ConnectionString));
                }
                
                if (Scenarios.Any("Raw") || Scenarios.Any("Dapper"))
                {
                    services.AddSingleton<DbProviderFactory>(NpgsqlFactory.Instance);
                }
            }
            else if (appSettings.Database == DatabaseServer.MySql)
            {
                if (Scenarios.Any("Raw") || Scenarios.Any("Dapper"))
                {
                    services.AddSingleton<DbProviderFactory>(MySqlClientFactory.Instance);
                }
            }

            if (Scenarios.Any("Ef"))
            {
                services.AddScoped<EfDb>();
            }

            if (Scenarios.Any("Raw"))
            {
                services.AddScoped<RawDb>();
            }

            if (Scenarios.Any("Fortunes"))
            {
                var settings = new TextEncoderSettings(UnicodeRanges.BasicLatin, UnicodeRanges.Katakana, UnicodeRanges.Hiragana);
                settings.AllowCharacter('\u2014');  // allow EM DASH through
                services.AddWebEncoders((options) =>
                {
                    options.TextEncoderSettings = settings;
                });
            }

            if (Scenarios.Any("Mvc"))
            {
                var mvcBuilder = services
                    .AddMvcCore()
                    .AddControllersAsServices();

                if (Scenarios.MvcJson || Scenarios.Any("MvcDbSingle") || Scenarios.Any("MvcDbMulti"))
                {
                    mvcBuilder.AddJsonFormatters();
                }

                if (Scenarios.MvcViews || Scenarios.Any("MvcDbFortunes"))
                {
                    mvcBuilder
                        .AddViews()
                        .AddRazorViewEngine();
                }
            }
        }

        public void Configure(IApplicationBuilder app)
        {
            if (Scenarios.Plaintext)
            {
                app.UsePlainText();
            }

            if (Scenarios.Json)
            {
                app.UseJson();
            }

            // Single query endpoints
            if (Scenarios.DbSingleQueryRaw)
            {
                app.UseSingleQueryRaw();
            }

            if (Scenarios.DbSingleQueryEf)
            {
                app.UseSingleQueryEf();
            }

            // Multiple query endpoints
            if (Scenarios.DbMultiQueryRaw)
            {
                app.UseMultipleQueriesRaw();
            }

            if (Scenarios.DbMultiQueryEf)
            {
                app.UseMultipleQueriesEf();
            }

            // Multiple update endpoints
            if (Scenarios.DbMultiUpdateRaw)
            {
                app.UseMultipleUpdatesRaw();
            }

            if (Scenarios.DbMultiUpdateEf)
            {
                app.UseMultipleUpdatesEf();
            }

            // Fortunes endpoints
            if (Scenarios.DbFortunesRaw)
            {
                app.UseFortunesRaw();
            }

            if (Scenarios.DbFortunesEf)
            {
                app.UseFortunesEf();
            }

            if (Scenarios.Any("Mvc"))
            {
                app.UseMvc();
            }

            if (Scenarios.Any("Update"))
            {
                BatchUpdateString.Initalize();
            }

            if (Scenarios.StaticFiles)
            {
                app.UseStaticFiles();
            }
        }
    }
}
