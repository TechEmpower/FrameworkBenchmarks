FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
RUN apt-get update
RUN apt-get -yqq install clang zlib1g-dev libkrb5-dev libtinfo5
WORKDIR /app
COPY PlatformBenchmarks .
RUN dotnet publish -c Release -o out -r linux-x64 /p:IsDatabase=true

FROM mcr.microsoft.com/dotnet/aspnet:5.0 AS runtime
ENV ASPNETCORE_URLS http://+:8080
WORKDIR /app
COPY --from=build /app/out ./
COPY PlatformBenchmarks/appsettings.postgresql.updates.json ./appsettings.json

EXPOSE 8080

ENTRYPOINT ["./PlatformBenchmarks"]
