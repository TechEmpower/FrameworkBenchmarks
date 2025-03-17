# Giraffe Benchmarks on Linux

This application tests Giraffe in 2 modes:

- Default: Using Giraffe's Endpoint Routing APIs with the `System.Text.Json` serializer
- Newtonsoft: Testing the JSON endpoint with the `NewtonsoftJson` serializer

## Infrastructure Software Versions

**Language**

* F# 8.0

**Platforms**

* .NET 6 (Windows and Linux)

**Web Servers**

* [Kestrel](https://github.com/aspnet/KestrelHttpServer)

**Web Stack**

* [Giraffe](https://github.com/giraffe-fsharp/Giraffe)
* ASP.NET Core

## Paths & Source for Tests

All source code is inside `Program.fs`.

App listens for a single command line argument to pick the desired JSON implementation:

    - `system`: `System.Text.Json`
    - `newtonsoft`: `Newtonsoft.Json`
