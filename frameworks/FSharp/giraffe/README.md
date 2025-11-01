# Giraffe Benchmarks on Linux

This application tests Giraffe in 3 modes:

- Default: Using Giraffe's Endpoint Routing APIs with the `System.Text.Json` serializer
- Newtonsoft: Testing the JSON endpoint with the `NewtonsoftJson` serializer
- FSharpFriendly: Testing the JSON endpoint with the `FSharp.SystemTextJson` serializer

## Infrastructure Software Versions

**Language**

* F# 9.0

**Platforms**

* .NET 9 (Windows and Linux)

**Web Servers**

* [Kestrel](https://github.com/dotnet/aspnetcore/tree/main/src/Servers/Kestrel)

**Web Stack**

* [Giraffe](https://github.com/giraffe-fsharp/Giraffe)
* ASP.NET Core

## Paths & Source for Tests

App listens for a single command line argument to pick the desired JSON implementation:

- `system`: `System.Text.Json`
- `newtonsoft`: `Newtonsoft.Json`
- `fsharpfriendly`: `FSharp.SystemTextJson`
