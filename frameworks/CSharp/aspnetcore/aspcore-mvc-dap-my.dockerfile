FROM mcr.microsoft.com/dotnet/sdk:6.0.100-rc.2 AS build
WORKDIR /app
COPY Benchmarks .
RUN dotnet publish -c Release -o out

FROM mcr.microsoft.com/dotnet/aspnet:6.0.0-rc.2 AS runtime
ENV ASPNETCORE_URLS http://+:8080

# Full PGO
ENV DOTNET_TieredPGO 1 
ENV DOTNET_TC_QuickJitForLoops 1 
ENV DOTNET_ReadyToRun 0

WORKDIR /app
COPY --from=build /app/out ./
COPY Benchmarks/appsettings.mysql.json ./appsettings.json

EXPOSE 8080

ENTRYPOINT ["dotnet", "Benchmarks.dll", "scenarios=MvcDbSingleQueryDapper,MvcDbMultiQueryDapper,MvcDbMultiUpdateDapper,MvcDbFortunesDapper"]
