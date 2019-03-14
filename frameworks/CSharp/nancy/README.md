# Nancy on Mono and CoreCLR Benchmarking Test

The information below contains information specific to Nancy on Mono and CoreCLR. 
For further guidance, review the 
[documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki). 
Also note the additional information provided in the [CSharp README](../).

This is the Nancy on Mono and CoreCLR portion of a [benchmarking test suite](../../) 
comparing a variety of web platforms.

## Infrastructure Software Versions

**Language**

* C# 7.2

**Platforms**

* Mono 5.12.X (Linux)
* CoreCLR 2.1 (Linux)

**Web Servers**

* Kestrel (Linux)

**Web Stack**

* ASP.NET Core 2.1
* Nancy 2.0.0

**Databases**

* MySQL Connector + Dapper (ORM)

**Developer Tools**

* Visual Studio 2017

## Paths & Source for Tests

* [Plaintext](src/PlainModule.cs): "/plaintext"
* [JSON Serialization](src/JsonModule.cs): "/json"

### Nancy - Dapper (ORM)

* [Single Database Query](src/DbModule.cs): "/db"
* [Multiple Database Queries](src/QueryModule.cs): "/query/10"

## Add a New Test for Nancy

### Platform Installation

[Install Mono on Linux](https://www.mono-project.com/download/stable/#download-lin)

[Install CoreCLR on Linux](hhttps://www.microsoft.com/net/download/linux/build)

## Get Help

### Experts

_There aren't any experts listed, yet. If you're an expert, add yourself!_

### Community

* Chat in the [#NancyFX](https://jabbr.net/account/login?ReturnUrl=%2F#/rooms/nancyfx) room on JabbR.
* #NancyFx on Twitter.

### Resources

* [Source Code](https://github.com/NancyFx/Nancy)
* [Issue #877 - Discussion regarding Event2.dll and Global.asax thread configuration](https://github.com/TechEmpower/FrameworkBenchmarks/issues/877)
