# ASP.NET MVC on Mono and Windows

## Tests

* JSON serialization
* Single query
* Multiple queries
* Server-side templates and collections
* Updates

## Versions

**Language**

* C# 5.0

**Platforms**

* .NET Framework 4.5 (Windows)
* Mono 3.0.10 (Linux)

**Web Servers**

* IIS 8 (Windows)
* XSP 2.11.0.0 (3.0?) (Linux)
* nginx 1.4.0 & XSP FastCGI (Linux)

**Web Stack**

* ASP.NET 4.5
* ASP.NET MVC Framework 4

**Databases**

* MySQL Connector/Net 6.7.2-beta ([custom build](https://github.com/pdonald/mysql-connector-net))
* Npgsql 2.0.12 ([custom build](https://github.com/pdonald/Npgsql2))
* Entity Framework 6.0.0-alpha3
* Mongo C# Driver 1.8.1
* SqlClient Data Provider for SQL Server

**Developer Tools**

* Visual Studio 2012 Update 2

## Mono Installation

    sudo apt-get install build-essential autoconf automake libtool zlib1g-dev git

    git clone git://github.com/mono/mono
    cd mono
    git checkout mono-3.10
    ./autogen.sh --prefix=/usr/local
    make get-monolite-latest
    make EXTERNAL_MCS=${PWD}/mcs/class/lib/monolite/gmcs.exe
    sudo make install

    cd ..

    git clone git://github.com/mono/xsp
    cd xsp
    git checkout 3.0
    ./autogen.sh --prefix=/usr/local
    make
    sudo make install