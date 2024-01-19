# Servicestack-V6 Tests on Windows and Linux
This includes tests for plaintext and json serialization.

## Infrastructure Software Versions

**Language**

* C# 10.0

**Platforms**

* .NET 6.0 (Windows and Linux)

**Web Servers**

* [Kestrel](https://github.com/dotnet/aspnetcore/tree/main/src/Servers/Kestrel)

**Web Stack**

* [ServiceStack](https://servicestack.net/)
* ASP.Net 6

## Paths & Source for Tests

* [Plaintext](Benchmarks/Services/MyServices.cs#L10): "http://localhost:8080/plaintext"
* [JSON Serialization](Benchmarks/Services/MyServices.cs#16): "http://localhost:8080/json"