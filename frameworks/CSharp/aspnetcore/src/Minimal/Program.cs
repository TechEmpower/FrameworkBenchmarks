// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System.Text.Encodings.Web;
using System.Text.Unicode;
using Microsoft.AspNetCore.Http.HttpResults;
using RazorSlices;
using Minimal;
using Minimal.Database;
using Minimal.Models;

var builder = WebApplication.CreateBuilder(args);

// Disable logging as this is not required for the benchmark
builder.Logging.ClearProviders();

builder.WebHost.ConfigureKestrel(options =>
{
     options.AllowSynchronousIO = true;
});

// Load custom configuration
var appSettings = new AppSettings();
builder.Configuration.Bind(appSettings);

// Add services to the container.
builder.Services.AddSingleton(new Db(appSettings));

var app = builder.Build();

app.MapGet("/plaintext", () => "Hello, World!");

app.MapGet("/plaintext/result", () => Results.Text("Hello, World!"));

app.MapGet("/json", () => new { message = "Hello, World!" });

app.MapGet("/db", async (Db db) => await db.LoadSingleQueryRow());

var createFortunesTemplate = RazorSlice.ResolveSliceFactory<List<Fortune>>("/Templates/Fortunes.cshtml");
var htmlEncoder = CreateHtmlEncoder();

app.MapGet("/fortunes", async (HttpContext context, Db db) => {
    var fortunes = await db.LoadFortunesRows();
    var template = (RazorSliceHttpResult<List<Fortune>>)createFortunesTemplate(fortunes);
    template.HtmlEncoder = htmlEncoder;
    return template;
});

app.MapGet("/queries/{count?}", async (Db db, string? count) => await db.LoadMultipleQueriesRows(count));

app.MapGet("/updates/{count?}", async (Db db, string? count) => await db.LoadMultipleUpdatesRows(count));

app.Lifetime.ApplicationStarted.Register(() => Console.WriteLine("Application started. Press Ctrl+C to shut down."));
app.Lifetime.ApplicationStopping.Register(() => Console.WriteLine("Application is shutting down..."));

app.Run();

static HtmlEncoder CreateHtmlEncoder()
{
    var settings = new TextEncoderSettings(UnicodeRanges.BasicLatin, UnicodeRanges.Katakana, UnicodeRanges.Hiragana);
    settings.AllowCharacter('\u2014'); // allow EM DASH through
    return HtmlEncoder.Create(settings);
}