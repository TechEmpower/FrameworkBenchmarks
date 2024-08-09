FROM mcr.microsoft.com/dotnet/sdk:8.0.100 AS build
RUN apt-get update
RUN apt-get -yqq install clang zlib1g-dev wget curl make libkrb5-dev libtinfo5

WORKDIR /odbc

RUN curl -L -o unixODBC-2.3.12.tar.gz ftp://ftp.unixodbc.org/pub/unixODBC/unixODBC-2.3.12.tar.gz
RUN tar -xvf unixODBC-2.3.12.tar.gz

WORKDIR /odbc/unixODBC-2.3.12
RUN ./configure --prefix=/usr/local/unixODBC --disable-dependency-tracking
RUN make
RUN make install

ENV PATH=/usr/local/unixODBC/lib:$PATH

WORKDIR /app
COPY src .
RUN dotnet publish -c Release -o out /p:Database=mysql


# Construct the actual image that will run
FROM mcr.microsoft.com/dotnet/aspnet:8.0.0 AS runtime

RUN apt-get update
# The following installs standard versions unixodbc and pgsqlodbc
# unixodbc still needs to be installed even if compiled locally
RUN apt-get install -y unixodbc wget curl odbc-postgresql --no-install-recommends gcc g++ gnupg
RUN apt-get update

WORKDIR /odbc

RUN curl -L -o mariadb-connector-odbc-3.2.2-debian-bookworm-amd64.tar.gz https://dlm.mariadb.com/3852001/Connectors/odbc/connector-odbc-3.2.2/mariadb-connector-odbc-3.2.2-debian-bookworm-amd64.tar.gz
RUN tar -xvzf mariadb-connector-odbc-3.2.2-debian-bookworm-amd64.tar.gz
RUN cp mariadb-connector-odbc-3.2.2-debian-bookworm-amd64/lib/mariadb/libm* /usr/lib/
RUN cp -r /odbc/mariadb-connector-odbc-3.2.2-debian-bookworm-amd64/lib/mariadb /usr/local/lib/mariadb
RUN rm mariadb-connector-odbc-3.2.2-debian-bookworm-amd64.tar.gz
#TODOLOCAL
#RUN curl -L -o mariadb-connector-odbc-3.2.2-debian-bookworm-aarch64.tar.gz https://downloads.mariadb.com/Connectors/odbc/connector-odbc-3.2.2/mariadb-connector-odbc-3.2.2-debian-bookworm-aarch64.tar.gz
#RUN tar -xvzf mariadb-connector-odbc-3.2.2-debian-bookworm-aarch64.tar.gz
#RUN cp mariadb-connector-odbc-3.2.2-debian-bookworm-aarch64/lib/mariadb/libm* /usr/lib/
#RUN cp -r /odbc/mariadb-connector-odbc-3.2.2-debian-bookworm-aarch64/lib/mariadb /usr/local/lib/mariadb
#RUN rm mariadb-connector-odbc-3.2.2-debian-bookworm-aarch64.tar.gz

COPY --from=build /usr/local/unixODBC /usr/local/unixODBC

ENV PATH=/usr/local/unixODBC/bin:$PATH

WORKDIR /etc/
COPY odbcinst.ini .
COPY ./odbcinst.ini /usr/local/unixODBC/etc


# Full PGO
ENV DOTNET_TieredPGO 1 
ENV DOTNET_TC_QuickJitForLoops 1 
ENV DOTNET_ReadyToRun 0

ENV ASPNETCORE_URLS http://+:8080
WORKDIR /app
COPY --from=build /app/out ./

RUN cp /usr/local/unixODBC/lib/libodbc.so /app
RUN cp /usr/local/unixODBC/lib/libodbc.so.2 /app
RUN cp /usr/lib/libm* /app

EXPOSE 8080

ENTRYPOINT ["./appMpower"]