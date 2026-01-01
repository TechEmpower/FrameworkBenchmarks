FROM mcr.microsoft.com/dotnet/sdk:10.0 AS build
RUN apt-get update
RUN apt-get -yqq install clang zlib1g-dev

WORKDIR /app
COPY src .
RUN dotnet publish -c Release -o out /p:Database=postgresql /p:Driver=odbc

# Construct the actual image that will run
FROM mcr.microsoft.com/dotnet/aspnet:10.0.1 AS runtime

RUN apt-get update
RUN apt-get install -y unixodbc-dev unixodbc odbc-postgresql
RUN apt-get update

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

#TODOGITHUB
RUN cp /usr/lib/x86_64-linux-gnu/libodbc* /app
#TODOLOCAL
#RUN cp /usr/lib/aarch64-linux-gnu/libodbc* /app

#TEST: ./tfb --test appmpower-odbc-pg  --type db

EXPOSE 8080

ENTRYPOINT ["./appMpower"]