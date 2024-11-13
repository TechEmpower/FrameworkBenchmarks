FROM mcr.microsoft.com/dotnet/sdk:9.0.100 AS build
RUN apt-get update
RUN apt-get -yqq install clang zlib1g-dev
RUN apt-get update

WORKDIR /app
COPY src .
RUN dotnet publish -c Release -o out /p:Database=mysql

# Construct the actual image that will run
FROM mcr.microsoft.com/dotnet/aspnet:9.0.0 AS runtime

RUN apt-get update
# The following installs standard versions unixodbc and pgsqlodbc
# unixodbc still needs to be installed even if compiled locally
RUN apt-get install -y unixodbc wget curl
RUN apt-get update

WORKDIR /odbc

RUN curl -L -o mariadb-connector-odbc-3.1.20-debian-bookworm-amd64.tar.gz https://downloads.mariadb.com/Connectors/odbc/connector-odbc-3.1.20/mariadb-connector-odbc-3.1.20-debian-bookworm-amd64.tar.gz
RUN tar -xvzf mariadb-connector-odbc-3.1.20-debian-bookworm-amd64.tar.gz
RUN cp mariadb-connector-odbc-3.1.20-debian-bookworm-amd64/lib/mariadb/libm* /usr/lib/
RUN cp -r /odbc/mariadb-connector-odbc-3.1.20-debian-bookworm-amd64/lib/mariadb /usr/local/lib/mariadb
RUN rm mariadb-connector-odbc-3.1.20-debian-bookworm-amd64.tar.gz
#TODOLOCAL
#RUN curl -L -o mariadb-connector-odbc-3.1.20-debian-bookworm-aarch64.tar.gz https://downloads.mariadb.com/Connectors/odbc/connector-odbc-3.1.20/mariadb-connector-odbc-3.1.20-debian-bookworm-aarch64.tar.gz
#RUN tar -xvzf mariadb-connector-odbc-3.1.20-debian-bookworm-aarch64.tar.gz
#RUN cp mariadb-connector-odbc-3.1.20-debian-bookworm-aarch64/lib/mariadb/libm* /usr/lib/
#RUN cp -r /odbc/mariadb-connector-odbc-3.1.20-debian-bookworm-aarch64/lib/mariadb /usr/local/lib/mariadb
#RUN rm mariadb-connector-odbc-3.1.20-debian-bookworm-aarch64.tar.gz

ENV PATH=/usr/local/unixODBC/bin:$PATH

WORKDIR /etc/
COPY odbcinst.ini .

# Full PGO
ENV DOTNET_TieredPGO 1 
ENV DOTNET_TC_QuickJitForLoops 1 
ENV DOTNET_ReadyToRun 0

ENV ASPNETCORE_URLS http://+:8080
WORKDIR /app
COPY --from=build /app/out ./

RUN cp /usr/lib/libm* /app

EXPOSE 8080

ENTRYPOINT ["./appMpower"]