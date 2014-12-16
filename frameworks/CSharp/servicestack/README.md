# ServiceStack on Mono and Windows

## Tests

### JSON Response

* `http://localhost:8080/json`

### Plain Text Response

* `http://localhost:8080/plaintext`

### Database Responses

**Microsoft SQL Server** using ORMLite

* `http://localhost:8080/sqlserver/db`
* `http://localhost:8080/sqlserver/queries/10`
* `http://localhost:8080/sqlserver/fortunes`
* `http://localhost:8080/sqlserver/updates/25`

**MySQL** using ORMLite

* `http://localhost:8080/mysql/db`
* `http://localhost:8080/mysql/queries/10`
* `http://localhost:8080/mysql/fortunes`
* `http://localhost:8080/mysql/updates/25`

**PostgreSQL** using ORMLite

* `http://localhost:8080/postgresql/db`
* `http://localhost:8080/postgresql/queries/10`
* `http://localhost:8080/postgresql/fortunes`
* `http://localhost:8080/postgresql/updates/25`

**MongoDB**

* `http://localhost:8080/mongodb/db`
* `http://localhost:8080/mongodb/queries/10`
* `http://localhost:8080/mongodb/fortunes`
* `http://localhost:8080/mongodb/updates/25`

---

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

* Self Hosting using HTTPListener (Windows/Linux)
* IIS 8 (Windows) - includes [Swagger](http://www.nuget.org/packages/ServiceStack.Api.Swagger/)
* nginx 1.4.0 & XSP FastCGI (Linux)

**Web Stack**

* ASP.NET 4.5
* [ServiceStack](https://github.com/servicestack/servicestack/wiki)

**Database Providers**

* Microsoft SQL Server - [interface](http://www.nuget.org/packages/ServiceStack.OrmLite.SqlServer/)
* MySQL - [driver](http://www.nuget.org/packages/MySql.Data/) + [interface](http://www.nuget.org/packages/ServiceStack.OrmLite.MySql/)
* PostgreSQL - [driver](http://www.nuget.org/packages/Npgsql/) + [interface](http://www.nuget.org/packages/ServiceStack.OrmLite.PostgreSQL/)
* MongoDB - [driver](http://www.nuget.org/packages/mongocsharpdriver/)

**Caching Providers**

* In-Memory
* Redis NoSQL Db - [client w/interface](http://www.nuget.org/packages/ServiceStack.Redis)
* MemCache - [client](http://www.nuget.org/packages/EnyimMemcached) + [interface](http://www.nuget.org/packages/ServiceStack.Caching.Memcached)
* Amazon Web Services In-Memory DynamoDb DataCache - [client](http://www.nuget.org/packages/AWSSDK) + [interface](http://www.nuget.org/packages/ServiceStack.Caching.AwsDynamoDb)
* Microsoft Azure In-Memory DataCache - [client](http://www.nuget.org/packages/WindowsAzure.Caching) + [interface](http://www.nuget.org/packages/ServiceStack.Caching.Azure)

**Developer Tools**

* Visual Studio 2012
