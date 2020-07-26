FROM mcr.microsoft.com/dotnet/core/sdk:3.1.101 AS build
WORKDIR /app
COPY PlatformBenchmarks .
RUN dotnet publish -c Release -o out /p:IsDatabase=true

FROM mcr.microsoft.com/dotnet/core/aspnet:3.1.2 AS runtime
ENV ASPNETCORE_URLS http://+:8080
WORKDIR /app
COPY --from=build /app/out ./
COPY PlatformBenchmarks/appsettings.postgresql.updates.json ./appsettings.json

ENTRYPOINT ["dotnet", "PlatformBenchmarks.dll", "KestrelTransport=LinuxTransport"]
