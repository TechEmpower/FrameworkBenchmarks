FROM mcr.microsoft.com/dotnet/core/sdk:2.2 AS build
WORKDIR /app
COPY Benchmarks .
RUN dotnet publish -c Release -o out

FROM mcr.microsoft.com/dotnet/core/aspnet:2.2 AS runtime
ENV ASPNETCORE_URLS http://+:8080
ENV COMPlus_ReadyToRun 0
WORKDIR /app
COPY --from=build /app/out ./
COPY Benchmarks/appsettings.mysql.json ./appsettings.json

ENTRYPOINT ["dotnet", "Benchmarks.dll", "scenarios=MvcDbSingleQueryRaw,MvcDbMultiQueryRaw,MvcDbMultiUpdateRaw,MvcDbFortunesRaw"]