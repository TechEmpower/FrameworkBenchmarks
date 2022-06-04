# [appMpower](https://github.com/LLT21/)(.Net) Benchmarking Test
This includes tests for plaintext, json, db, queries, updates and fortune.

[`appMpower`](https://github.com/LLT21/) is a nativily compiled (AOT) .NET implementation. The native compilation is done with reflection disabled; because the most used PostgreSQL .NET library is not reflection free, the PostgreSQL ODBC driver is used instead.

## Infrastructure Software Versions

**Language**

* C# 7.0

**Platforms**

* .NET Core (Windows and Linux)

**Web Servers**

* [Kestrel](https://github.com/aspnet/KestrelHttpServer)

**Web Stack**

* ASP.NET Core

## Paths & Source for Tests

* [Plaintext](Benchmarks/Program.cs): "/plaintext"
* [JSON Serialization](Benchmarks/Program.cs): "/json"
* [Single query](Benchmarks/Program.cs): "/db"
* [Multiple query](Benchmarks/Program.cs): "/queries"
* [Updates](Benchmarks/Program.cs): "/updates"
* [Fortune](Benchmarks/Program.cs): "/fortunes"
