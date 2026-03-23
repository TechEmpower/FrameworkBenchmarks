using Carter;
using Microsoft.AspNetCore.Builder;
using Microsoft.Extensions.Logging;

var builder = WebApplication.CreateBuilder(args);

// Disable logging as this is not required for the benchmark
builder.Logging.ClearProviders();
builder.Services.AddCarter();
var app = builder.Build();

app.MapCarter();
app.Run();