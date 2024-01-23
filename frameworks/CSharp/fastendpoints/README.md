# FastEndpoints Tests on Windows and Linux
This includes tests for plaintext and json serialization.

## Infrastructure Software Versions

**Language**

* C# 12.0

**Platforms**

* .NET 8 (Windows and Linux)

**Web Servers**

* [Kestrel](https://github.com/dotnet/aspnetcore/tree/main/src/Servers/Kestrel)

**Web Stack**

* [FastEndpoints](https://fast-endpoints.com/)
* ASP.NET 8

## Paths & Source for Tests

* [Plaintext](Benchmarks/Endpoints/PlainTextEndpoint.cs): "http://localhost:8080/plaintext"
* [JSON Serialization](Benchmarks/Endpoints/JsonEndpoint.cs): "http://localhost:8080/json"
