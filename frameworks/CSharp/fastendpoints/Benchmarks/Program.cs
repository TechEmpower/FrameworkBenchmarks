global using FastEndpoints;

var bld = WebApplication.CreateBuilder();
bld.Logging.ClearProviders();
bld.Services.AddFastEndpoints();

var app = bld.Build();
app.UseFastEndpoints();
app.Run("http://0.0.0.0:8080");