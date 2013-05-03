# Mono ASP.NET MVC

## Tests

### JSON

`http://localhost:8080/json`

### ADO.NET

#### MySQL

`http://localhost:8080/adonet/mysql`
`http://localhost:8080/adonet/mysql?queries=10`

#### PostgreSQL

`http://localhost:8080/adonet/postgresql`
`http://localhost:8080/adonet/postgresql?queries=10`

### MongoDB

`http://localhost:8080/mongodb`
`http://localhost:8080/mongodb?queries=10`

### Entity Framework

_Currently not working._

`http://localhost:8080/ef/mysql`
`http://localhost:8080/ef/mysql?queries=10`

`http://localhost:8080/ef/postgresql`
`http://localhost:8080/ef/postgresql?queries=10`

## Mono Installation

    sudo apt-get install build-essential autoconf libtool automake git

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
