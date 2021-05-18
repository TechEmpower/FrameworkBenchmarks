# EasyRpc Tests on Linux
This includes tests for plaintext and json serialization.

## Infrastructure Software Versions

**Language**

* C# 7.0

**Platforms**

* .NET Core (Windows and Linux)

**Web Servers**

* [Kestrel](https://github.com/aspnet/KestrelHttpServer)

**Web Stack**

* [EasyRpc](https://github.com/ipjohnson/EasyRpc)
* ASP.NET Core

## Paths & Source for Tests

* [Plaintext](Benchmarks/Startup.cs): "/plaintext"
* [JSON Serialization](Benchmarks/Startup.cs): "/json"
* [Single query](Benchmarks/Services/QueryService.cs): "/db"
* [Multiple query](Benchmarks/Services/QueryService.cs): "/queries"
* [Update query](Benchmarks/Services/QueryService.cs): "/updates"
* [Caching query](Benchmarks/Services/QueryService.cs): "/cached-worlds"
* [Fortune](Benchmarks/Services/FortuneService.cs): "/fortunes/fortunes"
