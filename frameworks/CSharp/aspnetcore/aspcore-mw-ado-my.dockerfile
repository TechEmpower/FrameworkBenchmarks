FROM mcr.microsoft.com/dotnet/core/sdk:3.1.101 AS build
WORKDIR /app
COPY Benchmarks .
RUN dotnet publish -c Release -o out

FROM mcr.microsoft.com/dotnet/core/aspnet:3.1.2 AS runtime
ENV ASPNETCORE_URLS http://+:8080
WORKDIR /app
COPY --from=build /app/out ./
COPY Benchmarks/appsettings.mysql.json ./appsettings.json

ENTRYPOINT ["dotnet", "Benchmarks.dll", "scenarios=DbSingleQueryRaw,DbMultiQueryRaw,DbMultiUpdateRaw,DbFortunesRaw"]