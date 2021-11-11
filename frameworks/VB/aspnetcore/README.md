# ASP.NET Core Tests on Windows and Linux

See [.NET Core](http://dot.net) and [ASP.NET Core](https://github.com/aspnet) for more information.

This includes tests for plaintext and json serialization.

## Infrastructure Software Versions

**Language**

* VB.NET 15

**Platforms**

* .NET Core (Windows and Linux)

**Web Servers**

* [Kestrel](https://github.com/aspnet/KestrelHttpServer)
* [HttpSys](https://github.com/aspnet/HttpSysServer)

**Web Stack**

* ASP.NET Core

## Paths & Source for Tests

* [Plaintext](Benchmarks/Middleware/PlaintextMiddleware.cs): "/plaintext"
* [JSON Serialization](Benchmarks/Middleware/JsonMiddleware.cs): "/json"
* [JSON Serialization Utf8Json](Benchmarks/Middleware/Utf8JsonMiddleware.cs): "/utf8json"
* [Single Query Raw](Benchmarks/Middleware/SingleQueryRawMiddleware.cs): "/db"
* [Multiple Queries Raw](Benchmarks/Middleware/MultipleQueriesRawMiddleware.cs): "/queries"
* [Data Updates Raw](Benchmarks/Middleware/MultipleUpdatesRawMiddleware.cs): "/updates"
* [Fortunes Raw](Benchmarks/Middleware/FortunesRawMiddleware.cs): "/fortunes"
