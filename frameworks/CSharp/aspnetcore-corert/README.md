# ASP.NET Core Tests on Windows and Linux

See [.NET CoreRT](https://github.com/dotnet/corert) and [ASP.NET Core](https://github.com/aspnet) for more information.

This includes tests for plaintext and json serialization.

## Infrastructure Software Versions

**Language**

* C# 7.0

**Platforms**

* .NET [CoreRT](https://github.com/dotnet/corert), a .NET Core runtime optimized for AOT (ahead of time compilation), with the accompanying .NET native compiler toolchain

**Web Servers**

* [Kestrel](https://github.com/aspnet/KestrelHttpServer)

**Web Stack**

* ASP.NET Core

## Paths & Source for Tests

* [Plaintext](PlatformBenchmarks/BenchmarkApplication.Plaintext.cs): "/plaintext"
* [JSON Serialization](PlatformBenchmarks/BenchmarkApplication.Json.cs): "/json"
