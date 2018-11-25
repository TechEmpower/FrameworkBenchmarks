# girrafe Tests on Linux
This includes tests for plaintext and json serialization.

## Infrastructure Software Versions

**Language**

* F# 4.1

**Platforms**

* .NET Core (Windows and Linux)

**Web Servers**

* [Kestrel](https://github.com/aspnet/KestrelHttpServer)

**Web Stack**

* [giraffe](https://github.com/giraffe-fsharp/Giraffe)
* ASP.NET Core

## Paths & Source for Tests

* [Plaintext](src/App/Stock.fs): "/plaintext"
* [Plaintext handwritten](src/App/Custom.fs): "/plaintext"
* [JSON serialization](src/App/Stock.fs): "/json"
* [JSON serialization via utf8json lib](src/App/Custom.fs): "/json"
* [Fortunes using Dapper](src/App/Stock.fs): "/fortunes"
* [Fortunes using Dapper and Custom renderer](src/App/Custom.fs): "/fortunes"

App listents for command line arguments to pick specific implementation. If "stock" passed as command line argument it will use out of the box handlers, otherwise will use custom ones.
