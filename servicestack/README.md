# ServiceStack on Mono and Windows

## Tests

### JSON Response

* `http://localhost:8080/json`

### Plain Text Response

* `http://localhost:8080/plaintext`

---

### ServiceStack - OrmLite (ORM)

**[Microsoft SQL Server](http://www.nuget.org/packages/ServiceStack.OrmLite.SqlServer)**

* `http://localhost:8080/sqlserver/db`
* `http://localhost:8080/sqlserver/queries?queries=10`
* `http://localhost:8080/sqlserver/fortunes`
* `http://localhost:8080/sqlserver/updates?queries=25`

**[MySQL](http://www.nuget.org/packages/ServiceStack.OrmLite.MySql)**

* `http://localhost:8080/mysql/db`
* `http://localhost:8080/mysql/queries?queries=10`
* `http://localhost:8080/mysql/fortunes`
* `http://localhost:8080/mysql/updates?queries=25`

**[PostgreSQL](http://www.nuget.org/packages/ServiceStack.OrmLite.PostgreSql)**

* `http://localhost:8080/postgresql/db`
* `http://localhost:8080/postgresql/queries?queries=10`
* `http://localhost:8080/postgresql/fortunes`
* `http://localhost:8080/postgresql/updates?queries=25`

**[MongoDB](http://www.nuget.org/packages/mongocsharpdriver)**

* `http://localhost:8080/mongodb/db`
* `http://localhost:8080/mongodb/queries?queries=10`
* `http://localhost:8080/mongodb/fortunes`
* `http://localhost:8080/mongodb/updates?queries=25`

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
* IIS 8 (Windows)
* nginx 1.4.0 & XSP FastCGI (Linux)

**Web Stack**

* ASP.NET 4.5
* [ServiceStack](https://github.com/servicestack/servicestack/wiki)

**Databases**

* Microsoft SQL Server 2005+
* [MySQL 5.x](http://www.nuget.org/packages/mysql.data) 
* [PostgreSQL 7.x](http://www.nuget.org/packages/Npgsql)

**Caching Providers**

* In-Memory
* Redis NoSQL Db - [redis]()  [ServiceStack package](http://www.nuget.org/packages/ServiceStack.Redis)
* MemCache - [memcache](http://www.nuget.org/packages/EnyimMemcached)  [ServiceStack package](http://www.nuget.org/packages/ServiceStack.Caching.Memcached)
* Amazon Web Services In-Memory DynamoDb DataCache - [aws](http://www.nuget.org/packages/AWSSDK)  [ServiceStack package](http://www.nuget.org/packages/ServiceStack.Caching.AwsDynamoDb)
* Microsoft Azure In-Memory DataCache - [azure](http://www.nuget.org/packages/WindowsAzure.Caching)  [ServiceStack package](http://www.nuget.org/packages/ServiceStack.Caching.Azure)

**Developer Tools**

* Visual Studio 2012
