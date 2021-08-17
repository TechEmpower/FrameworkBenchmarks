FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
RUN apt-get update
RUN apt-get -yqq install clang zlib1g-dev libkrb5-dev libtinfo5
RUN apt-get install -y make build-essential libssl-dev zlib1g-dev libbz2-dev \
   libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev \
   xz-utils tk-dev libffi-dev liblzma-dev pgpool2 vim-tiny

WORKDIR /odbc

RUN curl -L -o unixODBC-2.3.9.tar.gz ftp://ftp.unixodbc.org/pub/unixODBC/unixODBC-2.3.9.tar.gz
RUN tar -xvf unixODBC-2.3.9.tar.gz

WORKDIR /odbc/unixODBC-2.3.9
RUN ./configure --prefix=/usr/local/unixODBC
RUN make
RUN make install

ENV PATH=/usr/local/unixODBC/lib:$PATH

WORKDIR /app
COPY src .
RUN dotnet publish -c Release -o out -r linux-x64 /p:Database=mysql

# Construct the actual image that will run
FROM mcr.microsoft.com/dotnet/aspnet:5.0 AS runtime

RUN apt-get update
# The following installs standard versions unixodbc 2.3.6 and pgsqlodbc 11
#RUN apt-get install -y unixodbc odbc-postgresql
# unixodbc still needs to be installed even if compiled locally
RUN apt-get install -y unixodbc wget curl

WORKDIR /odbc

RUN curl -L -o mariadb-connector-odbc-3.1.13-debian-9-stretch-amd64.tar.gz https://downloads.mariadb.com/Connectors/odbc/connector-odbc-3.1.13/mariadb-connector-odbc-3.1.13-debian-9-stretch-amd64.tar.gz
RUN tar -xvzf mariadb-connector-odbc-3.1.13-debian-9-stretch-amd64.tar.gz
RUN cp mariadb-connector-odbc-3.1.13-debian-9-stretch-amd64/lib/mariadb/libm* /usr/lib/

COPY --from=build /usr/local/unixODBC /usr/local/unixODBC

# Check unixODBC version by: 
# 1. Logging into containter: docker run --rm -it --entrypoint=/bin/bash techempower/tfb.test.appmpower
# 2. odbcinst  --version

ENV PATH=/usr/local/unixODBC/bin:$PATH

WORKDIR /etc/
COPY odbcinst.ini .

WORKDIR /app
COPY --from=build /app/out ./

EXPOSE 8080

ENTRYPOINT ["./appMpower"]