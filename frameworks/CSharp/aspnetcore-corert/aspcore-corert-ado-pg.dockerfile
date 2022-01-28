FROM mcr.microsoft.com/dotnet/sdk:6.0.100 AS build
RUN apt-get update
RUN apt-get -yqq install clang zlib1g-dev libkrb5-dev
WORKDIR /app
COPY PlatformBenchmarks .
RUN dotnet publish -c Release -o out /p:DatabaseProvider=Npgsql -r linux-x64

FROM mcr.microsoft.com/dotnet/aspnet:6.0.0 AS runtime
ENV ASPNETCORE_URLS http://+:8080

WORKDIR /app
COPY --from=build /app/out ./
COPY PlatformBenchmarks/appsettings.postgresql.json ./appsettings.json

EXPOSE 8080

ENTRYPOINT ["./PlatformBenchmarks"]
