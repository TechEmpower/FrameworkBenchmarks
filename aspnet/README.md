# ASP.NET MVC on Windows and Mono

## Tests

* JSON serialization
* Single database query
* Multiple database queries
* Server-side templates and collections
* Database updates
* Plaintext

## Versions

**Language**

* C# 5.0

**Platforms**

* .NET Framework 4.5 (Windows)
* Mono 3.2.1 (Linux)

**Web Servers**

* IIS 8 (Windows)
* XSP latest (Linux)
* nginx 1.4.1 & XSP FastCGI (Linux)

**Web Stack**

* ASP.NET 4.5
* ASP.NET MVC Framework 4

**Databases**

* MySQL Connector/Net 6.7.2-beta ([custom build](https://github.com/pdonald/mysql-connector-net))
* Npgsql 2.0.13-beta1
* Entity Framework 6.0.0-beta1
* Mongo C# Driver 1.8.2

**Developer Tools**

* Visual Studio 2012

## Mono Installation

    sudo apt-get install git-core build-essential autoconf automake libtool zlib1g-dev pkg-config

    git clone git://github.com/mono/mono
    cd mono
    git checkout mono-3.2.1
    ./autogen.sh --prefix=/usr/local
    make get-monolite-latest
    make EXTERNAL_MCS=${PWD}/mcs/class/lib/monolite/gmcs.exe
    sudo make install

    cd ..

    git clone git://github.com/mono/xsp
    cd xsp
    ./autogen.sh --prefix=/usr/local
    make
    sudo make install
    
    mozroots --import --sync
