var builder = WebApplication.CreateBuilder(args);

builder.Services.AddMemoryCache();

ConnectionString.Set(builder.Configuration.GetConnectionString("BenchmarkConnection"));

var app = builder.Build();

SetCodeBehind.CodeBehindCompiler.Initialization();

app.UseCodeBehind();

app.Run();
