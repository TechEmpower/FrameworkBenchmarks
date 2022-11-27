FROM mcr.microsoft.com/dotnet/sdk:7.0.100 AS build
RUN apt-get update
RUN apt-get -yqq install clang zlib1g-dev
WORKDIR /app
COPY PlatformBenchmarks .
RUN dotnet publish -c Release -o out -p:PublishAot=true -p:DatabaseProvider=Npgsql

FROM mcr.microsoft.com/dotnet/aspnet:7.0.0 AS runtime
ENV ASPNETCORE_URLS http://+:8080

WORKDIR /app
COPY --from=build /app/out ./
COPY PlatformBenchmarks/appsettings.postgresql.json ./appsettings.json

EXPOSE 8080

ENTRYPOINT ["./PlatformBenchmarks"]
