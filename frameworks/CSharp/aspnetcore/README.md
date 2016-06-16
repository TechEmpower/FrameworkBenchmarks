# ASP.NET Core Tests on Windows and Linux

See [.NET Core](http://dot.net) and [ASP.NET Core](https://github.com/aspnet) for more information.

This includes tests for plaintext and json serialization.

## Infrastructure Software Versions

**Language**

* C# 6.0

**Platforms**

* .NET Core (Windows and Linux)

**Web Servers**

* [Kestrel](https://github.com/aspnet/kestrelHttpServer)
* WebListener

**Web Stack**

* ASP.NET Core
* ASP.NET Core MVC

## Paths & Source for Tests

* [Plaintext](Benchmarks/Middleware/PlaintextMiddleware.cs): "/plaintext"
* [Plaintext MVC](Benchmarks/Controllers/HomeController.cs): "/mvc/plaintext"
* [JSON Serialization](Benchmarks/Middleware/JsonMiddleware.cs): "/json"
* [JSON Serialization MVC](Benchmarks/Controllers/HomeController.cs): "/mvc/json"