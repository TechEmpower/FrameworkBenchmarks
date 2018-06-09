# ASP.NET MVC on Windows and Mono

## Note

Some tests have been removed from the `benchmark_config.json`. The code still exists in src. If you'd like to attempt to reimplement these tests, you can look to this [benchmark_config](https://github.com/TechEmpower/FrameworkBenchmarks/blob/master/frameworks/CSharp/aspnet/benchmark_config.json)

## Tests

* JSON serialization
* Single database query
* Multiple database queries
* Server-side templates and collections
* Database updates
* Plaintext

## Versions

**Language**

* C# 5.0

**Platforms**

* .NET Framework 4.5 (Windows)
* Mono 3.99.0 (Linux)

**Web Servers**

* IIS 8 (Windows)
* XSP latest (Linux)
* nginx 1.4.1 & XSP FastCGI (Linux)

**Web Stack**

* ASP.NET 4.5
* ASP.NET MVC Framework 5.2.2
* ASP.NET Razor 3.2.2

**Databases**

* MySQL Connector/Net 6.9.5
* Npgsql 2.2.3
* Entity Framework 6.1.1
* Mongo C# Driver 1.9.2

**Developer Tools**

* Visual Studio 2012
