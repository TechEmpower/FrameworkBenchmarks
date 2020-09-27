# ASP.NET Core Source Generator Test

See [.NET Core](http://dot.net) and [ASP.NET Core](https://github.com/aspnet) for more information.

Using C# source generators to transform `Route` [annotated methods](PlatformBenchmarks/BenchmarkApplication.cs)
into a Platform level test.

## Infrastructure Software Versions

**Language**

* C# 9.0

**Platforms**

* .NET 5.0 (Windows and Linux)

**Web Servers**

* [Kestrel](https://github.com/dotnet/aspnetcore/tree/master/src/Servers/Kestrel)

**Web Stack**

* ASP.NET Core
* [Ben.AspNetCore.PlatformExtensions](https://github.com/benaadams/Ben.AspNetCore.PlatformExtensions)

## Paths & Source for Tests

All tests are in a single file

* [Plaintext](PlatformBenchmarks/BenchmarkApplication.cs): "/plaintext"
* [JSON Serialization](PlatformBenchmarks/BenchmarkApplication.cs): "/json"
* [Single Query Raw](PlatformBenchmarks/BenchmarkApplication.cs): "/db"
* [Multiple Queries Raw](PlatformBenchmarks/BenchmarkApplication.cs): "/queries/"
* [Data Updates Raw](PlatformBenchmarks/BenchmarkApplication.cs): "/updates/"
