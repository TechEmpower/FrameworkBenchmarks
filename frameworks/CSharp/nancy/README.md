# Nancy on Mono and Windows Benchmarking Test

The information below contains information specific to Nancy on Mono and Windows. 
For further guidance, review the 
[documentation](http://frameworkbenchmarks.readthedocs.org/en/latest/). 
Also note the additional information provided in the [CSharp README](../).

This is the Nancy on Mono and Windows portion of a [benchmarking test suite](../../) 
comparing a variety of web platforms.

## Infrastructure Software Versions

**Language**

* C# 5.0

**Platforms**

* .NET Framework 4.5 (Windows)
* Mono 3.0.X (Linux)

**Web Servers**

* IIS 8 (Windows)
* nginx 1.4.0 & XSP FastCGI (Linux)

**Web Stack**

* ASP.NET 4.5
* Nancy 0.17.1 (custom build to address this issue: https://github.com/NancyFx/Nancy/pull/1100)

**Databases**

* MySQL Connector/Net

**Developer Tools**

* Visual Studio 2012

## Paths & Source for Tests

* [JSON Serialization](NancyModules/JsonModule.cs): "/json"

### Nancy - Dapper (ORM)

* [Single Database Query](NancyModules/DbModule.cs): "/db"
* [Multiple Database Queries](NancyModules/DbModule.cs): "/db/10"

## Add a New Test for Nancy

### Mono Installation

    sudo apt-get install build-essential autoconf automake libtool zlib1g-dev git

    git clone git://github.com/mono/mono
    cd mono
    git checkout mono-3.0.10
    ./autogen.sh --prefix=/usr/local
    make get-monolite-latest
    make EXTERNAL_MCS=${PWD}/mcs/class/lib/monolite/basic.exe
    sudo make install

    cd ..

    git clone git://github.com/mono/xsp
    cd xsp
    git checkout 3.0
    ./autogen.sh --prefix=/usr/local
    make
    sudo make install

## Get Help

### Experts

_There aren't any experts listed, yet. If you're an expert, add yourself!_

### Community

* Chat in the [#NancyFX](https://jabbr.net/account/login?ReturnUrl=%2F#/rooms/nancyfx) room on JabbR.
* #NancyFx on Twitter.

### Resources

* [Source Code](https://github.com/NancyFx/Nancy)
* [Issue #877 - Discussion regarding Event2.dll and Global.asax thread configuration](https://github.com/TechEmpower/FrameworkBenchmarks/issues/877)
