FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
RUN apt-get update
RUN apt-get -yqq install clang zlib1g-dev libkrb5-dev libtinfo5

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

RUN curl -L -o mysql-connector-odbc-8.0.26-linux-glibc2.12-x86-64bit.tar.gz https://dev.mysql.com/get/Downloads/Connector-ODBC/8.0/mysql-connector-odbc-8.0.26-linux-glibc2.12-x86-64bit.tar.gz
RUN tar -xf mysql-connector-odbc-8.0.26-linux-glibc2.12-x86-64bit.tar.gz
RUN cp mysql-connector-odbc-8.0.26-linux-glibc2.12-x86-64bit/lib/libmyodbc* /usr/lib/x86_64-linux-gnu/odbc/

WORKDIR /odbc

RUN curl -L -o mariadb-connector-odbc-3.1.13-debian-9-stretch-amd64.tar.gz https://downloads.mariadb.com/Connectors/odbc/connector-odbc-3.1.13/mariadb-connector-odbc-3.1.13-debian-9-stretch-amd64.tar.gz
RUN tar -xvzf mariadb-connector-odbc-3.1.13-debian-9-stretch-amd64.tar.gz
RUN cp mariadb-connector-odbc-3.1.13-debian-9-stretch-amd64/lib/mariadb/libm* /usr/lib/

#COPY --from=build /usr/local/unixODBC /usr/local/unixODBC

# Check unixODBC version by: 
# 1. Logging into containter: docker run --rm -it --entrypoint=/bin/bash techempower/tfb.test.appmpower
# 2. odbcinst  --version

#ENV PATH=/usr/local/unixODBC/bin:$PATH

WORKDIR /etc/
COPY odbcinst.ini .

WORKDIR /app
COPY --from=build /app/out ./

EXPOSE 8080

ENTRYPOINT ["./appMpower"]