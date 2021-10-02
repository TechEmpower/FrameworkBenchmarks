FROM mcr.microsoft.com/dotnet/sdk:6.0.100-rc.1 AS build
RUN apt-get update
RUN apt-get -yqq install clang zlib1g-dev libkrb5-dev libtinfo5
RUN apt-get install -y make build-essential libssl-dev zlib1g-dev libbz2-dev \
   libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev \
   xz-utils tk-dev libffi-dev liblzma-dev pgpool2 vim-tiny

WORKDIR /odbc

# To compile the latest postgresql odbc driver, postgresql itself needs to be installed
RUN curl -L -o postgresql-13.4.tar.gz https://ftp.postgresql.org/pub/source/v13.4/postgresql-13.4.tar.gz
RUN curl -L -o unixODBC-2.3.9.tar.gz ftp://ftp.unixodbc.org/pub/unixODBC/unixODBC-2.3.9.tar.gz
RUN curl -L -o psqlodbc-13.02.0000.tar.gz https://ftp.postgresql.org/pub/odbc/versions/src/psqlodbc-13.02.0000.tar.gz

RUN tar -xvf postgresql-13.4.tar.gz
RUN tar -xvf unixODBC-2.3.9.tar.gz
RUN tar -xvf psqlodbc-13.02.0000.tar.gz

WORKDIR /odbc/postgresql-13.4
RUN ./configure
RUN make
RUN make install

ENV PATH=/usr/local/pgsql/bin:$PATH

WORKDIR /odbc/unixODBC-2.3.9
RUN ./configure --prefix=/usr/local/unixODBC
RUN make
RUN make install

ENV PATH=/usr/local/unixODBC/lib:$PATH

WORKDIR /odbc/psqlodbc-13.02.0000
RUN ./configure --with-unixodbc=/usr/local/unixODBC --with-libpq=/usr/local/pgsql --prefix=/usr/local/pgsqlodbc
RUN make
RUN make install

WORKDIR /app
COPY src .
RUN dotnet publish -c Release -o out -r linux-x64  /p:Database=postgresql

# Construct the actual image that will run
FROM mcr.microsoft.com/dotnet/aspnet:6.0.0-rc.1 AS runtime

RUN apt-get update
# The following installs standard versions unixodbc 2.3.6 and pgsqlodbc 11
#RUN apt-get install -y unixodbc odbc-postgresql
# unixodbc still needs to be installed even if compiled locally
RUN apt-get install -y unixodbc wget curl libpq-dev build-essential

WORKDIR /odbc

RUN curl -L -o pgpool-II-4.2.3.tar.gz https://www.pgpool.net/mediawiki/download.php?f=pgpool-II-4.2.3.tar.gz
RUN tar -xvf pgpool-II-4.2.3.tar.gz

WORKDIR /odbc/pgpool-II-4.2.3
RUN ./configure
RUN make
RUN make install

COPY --from=build /usr/local/unixODBC /usr/local/unixODBC

# Check unixODBC version by: 
# 1. Logging into containter: docker run --rm -it --entrypoint=/bin/bash techempower/tfb.test.appmpower
# 2. odbcinst  --version

ENV PATH=/usr/local/unixODBC/bin:$PATH

COPY --from=build /usr/local/pgsqlodbc /usr/local/pgsqlodbc

WORKDIR /etc/
COPY odbcinst.ini .

WORKDIR /app
COPY --from=build /app/out ./

EXPOSE 8080

ENTRYPOINT ["./appMpower"]