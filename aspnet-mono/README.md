# Mono ASP.NET MVC

## Tests

### JSON

`http://localhost:8080/json`

### ADO.NET with MySQL

`http://localhost:8080/mysql-raw?queries=10`

### Entity Framework with MySQL

_Currently not working._

`http://localhost:8080/mysql-ef?queries=10`

## Install Mono

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
