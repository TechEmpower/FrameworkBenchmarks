# ASP.NET Core Tests on Mono and Linux

See [ASP.NET Core](https://github.com/aspnet) and [Mono](https://www.mono-project.com/) for more information.

This includes tests for plaintext and json serialization.

## Infrastructure Software Versions

**Language**

* C# 7.2

**Platforms**

* Mono (Linux)

**Web Servers**

* [Kestrel](https://github.com/aspnet/KestrelHttpServer)

**Web Stack**

* ASP.NET Core
* ASP.NET Core MVC

## Paths & Source for Tests

### Platform

* [Plaintext](PlatformBenchmarks/BenchmarkApplication.Plaintext.cs): "/plaintext"
* [JSON Serialization](PlatformBenchmarks/BenchmarkApplication.Json.cs): "/json"
* [Fortunes](PlatformBenchmarks/BenchmarkApplication.Fortunes.cs): "/fortunes"

### Framework

* [Plaintext](Benchmarks/Middleware/PlaintextMiddleware.cs): "/plaintext"
* [Plaintext MVC](Benchmarks/Controllers/HomeController.cs): "/mvc/plaintext"
* [JSON Serialization](Benchmarks/Middleware/JsonMiddleware.cs): "/json"
* [JSON Serialization MVC](Benchmarks/Controllers/HomeController.cs): "/mvc/json"
* [Single Query Raw](Benchmarks/Middleware/SingleQueryRawMiddleware.cs): "/db/raw"
* [Single Query EF](Benchmarks/Middleware/SingleQueryEfMiddleware.cs): "/db/ef"
* [Single Query MVC Raw](Benchmarks/Controllers/SingleQueryController.cs): "/mvc/db/raw"
* [Single Query MVC EF](Benchmarks/Controllers/SingleQueryController.cs): "/mvc/db/ef"
* [Multiple Queries Raw](Benchmarks/Middleware/MultipleQueriesRawMiddleware.cs): "/queries/raw"
* [Multiple Queries EF](Benchmarks/Middleware/MultipleQueriesEfMiddleware.cs): "/queries/ef"
* [Multiple Queries MVC Raw](Benchmarks/Controllers/MultipleQueriesController.cs): "/mvc/queries/raw"
* [Multiple Queries MVC EF](Benchmarks/Controllers/MultipleQueriesController.cs): "/mvc/queries/ef"
* [Data Updates Raw](Benchmarks/Middleware/MultipleUpdatesRawMiddleware.cs): "/updates/raw"
* [Data Updates EF](Benchmarks/Middleware/MultipleUpdatesEfMiddleware.cs): "/updates/ef"
* [Data Updates MVC Raw](Benchmarks/Controllers/MultipleUpdatesController.cs): "/mvc/updates/raw"
* [Data Updates MVC EF](Benchmarks/Controllers/MultipleUpdatesController.cs): "/mvc/updates/ef"
* [Fortunes Raw](Benchmarks/Middleware/FortunesRawMiddleware.cs): "/fortunes/raw"
* [Fortunes EF](Benchmarks/Middleware/FortunesEfMiddleware.cs): "/fortunes/ef"
* [Fortunes MVC Raw](Benchmarks/Controllers/FortunesController.cs): "/mvc/fortunes/raw"
* [Fortunes MVC EF](Benchmarks/Controllers/FortunesController.cs): "/mvc/fortunes/ef"
