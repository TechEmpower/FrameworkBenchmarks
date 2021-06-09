FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
WORKDIR /app
COPY PlatformBenchmarks .
RUN dotnet publish -c Release -o out /p:DatabaseProvider=Npgsql

FROM mcr.microsoft.com/dotnet/aspnet:5.0 AS runtime
ENV ASPNETCORE_URLS http://+:8080
WORKDIR /app
COPY --from=build /app/out ./
COPY PlatformBenchmarks/appsettings.postgresql.updates.json ./appsettings.json

EXPOSE 8080

ENTRYPOINT ["dotnet", "PlatformBenchmarks.dll"]
