global using FastEndpoints;

var builder = WebApplication.CreateBuilder();
builder.Logging.ClearProviders();
builder.Services.AddFastEndpoints();

var app = builder.Build();
app.UseFastEndpoints();
app.Run("http://0.0.0.0:8080");
