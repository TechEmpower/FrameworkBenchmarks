# ServiceStack on Mono and Windows

## Tests

### JSON

* `http://localhost:8080/json`

---

### ServiceStack - OrmLite (ORM)

**MySQL**

* `http://localhost:8080/db`
* `http://localhost:8080/db?queries=10`

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
* Mono 3.0.10 (Linux)

**Web Servers**

* IIS 8 (Windows)
* nginx 1.4.0 & XSP FastCGI (Linux)

**Web Stack**

* ASP.NET 4.5
* ServiceStack

**Databases**

* MySQL Connector/Net 6.7.2-beta (custom build)

**Developer Tools**

* Visual Studio 2012 Update 2
