# Mono ASP.NET MVC

## Tests

### JSON

`http://localhost:8080/json`

---

### ADO.NET (Raw)

**MySQL**

`http://localhost:8080/adonet/mysql`
`http://localhost:8080/adonet/mysql?queries=10`
`http://localhost:8080/adonet/mysql/fortunes`

**PostgreSQL**

`http://localhost:8080/adonet/postgresql`
`http://localhost:8080/adonet/postgresql?queries=10`
`http://localhost:8080/adonet/postgresql/fortunes`

---

### Entity Framework (ORM)

**MySQL**

`http://localhost:8080/entityframework/mysql`
`http://localhost:8080/entityframework/mysql?queries=10`
`http://localhost:8080/entityframework/mysql/fortunes`

---

### MongoDB

`http://localhost:8080/mongodb`
`http://localhost:8080/mongodb?queries=10`

## Mono Installation

    sudo apt-get install build-essential autoconf automake libtool zlib1g-dev git

    git clone git://github.com/mono/mono
    cd mono
    git checkout mono-3.10
    ./autogen.sh --prefix=/usr/local --with-profile2=no --with-profile4=no
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

