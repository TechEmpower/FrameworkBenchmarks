FROM mcr.microsoft.com/dotnet/sdk:9.0.100 AS build
RUN apt-get update
RUN apt-get -yqq install clang zlib1g-dev libkrb5-dev libtinfo5

WORKDIR /app
COPY src .
RUN dotnet publish -c Release -o out /p:Database=postgresql

# Construct the actual image that will run
FROM mcr.microsoft.com/dotnet/aspnet:9.0.0 AS runtime

RUN apt-get update
RUN apt-get install -y unixodbc-dev unixodbc odbc-postgresql
# unixodbc still needs to be installed even if compiled locally

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

#RUN cp /usr/lib/aarch64-linux-gnu/libodbc* /app
RUN cp /usr/lib/x86_64-linux-gnu/libodbc* /app


EXPOSE 8080

ENTRYPOINT ["./appMpower"]