# ASP.NET Core Tests on Windows and Linux

See [.NET Core](http://dot.net) and [ASP.NET Core](https://github.com/aspnet) for more information.

This includes tests for plaintext and json serialization.

## Infrastructure Software Versions

**Language**

* C# 11.0

**Platforms**

* .NET 7 (Windows and Linux)

**Web Servers**

* [Kestrel](https://github.com/aspnet/KestrelHttpServer)
* [HttpSys](https://github.com/aspnet/HttpSysServer)

**Web Stack**

* ASP.NET Core
* ASP.NET Core MVC

## Paths & Source for Tests

* [Plaintext](Benchmarks/Middleware/PlaintextMiddleware.cs): "/plaintext"
* [Plaintext MVC](Benchmarks/Controllers/HomeController.cs): "/mvc/plaintext"
* [JSON Serialization](Benchmarks/Middleware/JsonMiddleware.cs): "/json"
* [JSON Serialization MVC](Benchmarks/Controllers/HomeController.cs): "/mvc/json"
* [JSON Serialization Utf8Json](Benchmarks/Middleware/Utf8JsonMiddleware.cs): "/utf8json"
* [JSON Serialization SpanJson](Benchmarks/Middleware/SpanJsonMiddleware.cs): "/spanjson"
* [Single Query Raw](Benchmarks/Middleware/SingleQueryRawMiddleware.cs): "/db/raw"
* [Single Query EF](Benchmarks/Middleware/SingleQueryEfMiddleware.cs): "/db/ef"
* [Single Query LinqToDB](Benchmarks/Middleware/SingleQueryLinqToDBMiddleware.cs): "/db/linq2db"
* [Single Query Dapper](Benchmarks/Middleware/SingleQueryDapperMiddleware.cs): "/db/dapper"
* [Single Query MVC Raw](Benchmarks/Controllers/SingleQueryController.cs): "/mvc/db/raw"
* [Single Query MVC EF](Benchmarks/Controllers/SingleQueryController.cs): "/mvc/db/ef"
* [Single Query MVC LinqToDB](Benchmarks/Controllers/SingleQueryController.cs): "/mvc/db/linq2db"
* [Single Query MVC Dapper](Benchmarks/Controllers/SingleQueryController.cs): "/mvc/db/dapper"
* [Multiple Queries Raw](Benchmarks/Middleware/MultipleQueriesRawMiddleware.cs): "/queries/raw"
* [Multiple Queries EF](Benchmarks/Middleware/MultipleQueriesEfMiddleware.cs): "/queries/ef"
* [Multiple Queries LinqToDB](Benchmarks/Middleware/MultipleQueriesLinqToDBMiddleware.cs): "/queries/linq2db"
* [Multiple Queries Dapper](Benchmarks/Middleware/MultipleQueriesDapperMiddleware.cs): "/queries/dapper"
* [Multiple Queries MVC Raw](Benchmarks/Controllers/MultipleQueriesController.cs): "/mvc/queries/raw"
* [Multiple Queries MVC EF](Benchmarks/Controllers/MultipleQueriesController.cs): "/mvc/queries/ef"
* [Multiple Queries MVC LinqToDB](Benchmarks/Controllers/MultipleQueriesController.cs): "/mvc/queries/linq2db"
* [Multiple Queries MVC Dapper](Benchmarks/Controllers/MultipleQueriesController.cs): "/mvc/queries/dapper"
* [Data Updates Raw](Benchmarks/Middleware/MultipleUpdatesRawMiddleware.cs): "/updates/raw"
* [Data Updates EF](Benchmarks/Middleware/MultipleUpdatesEfMiddleware.cs): "/updates/ef"
* [Data Updates LinqToDB](Benchmarks/Middleware/MultipleUpdatesLinqToDBMiddleware.cs): "/updates/linq2db"
* [Data Updates Dapper](Benchmarks/Middleware/MultipleUpdatesDapperMiddleware.cs): "/updates/dapper"
* [Data Updates MVC Raw](Benchmarks/Controllers/MultipleUpdatesController.cs): "/mvc/updates/raw"
* [Data Updates MVC EF](Benchmarks/Controllers/MultipleUpdatesController.cs): "/mvc/updates/ef"
* [Data Updates MVC LinqToDB](Benchmarks/Controllers/MultipleUpdatesController.cs): "/mvc/updates/linq2db"
* [Data Updates MVC Dapper](Benchmarks/Controllers/MultipleUpdatesController.cs): "/mvc/updates/dapper"
* [Fortunes Raw](Benchmarks/Middleware/FortunesRawMiddleware.cs): "/fortunes/raw"
* [Fortunes EF](Benchmarks/Middleware/FortunesEfMiddleware.cs): "/fortunes/ef"
* [Fortunes LinqToDB](Benchmarks/Middleware/FortunesLinqToDBMiddleware.cs): "/fortunes/linq2db"
* [Fortunes Dapper](Benchmarks/Middleware/FortunesDapperMiddleware.cs): "/fortunes/dapper"
* [Fortunes MVC Raw](Benchmarks/Controllers/FortunesController.cs): "/mvc/fortunes/raw"
* [Fortunes MVC EF](Benchmarks/Controllers/FortunesController.cs): "/mvc/fortunes/ef"
* [Fortunes MVC LinqToDB](Benchmarks/Controllers/FortunesController.cs): "/mvc/fortunes/linq2db"
* [Fortunes MVC Dapper](Benchmarks/Controllers/FortunesController.cs): "/mvc/fortunes/dapper"
