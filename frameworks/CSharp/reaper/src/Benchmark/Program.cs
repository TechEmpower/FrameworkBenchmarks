using Reaper;

var builder = WebApplication.CreateSlimBuilder(args);
builder.Logging.ClearProviders();
builder.Logging.Configure(o => o.ActivityTrackingOptions = ActivityTrackingOptions.None);
builder.UseReaper();

var app = builder.Build();

app.UseReaperMiddleware();
app.MapReaperEndpoints();

app.Run();