# Nancy on Mono and Windows

## Tests

### JSON

* `http://localhost:8080/json`

---

### Nancy - Dapper (ORM)

**MySQL**

* `http://localhost:8080/db`
* `http://localhost:8080/db/10`

## Mono Installation

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

## Versions

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
