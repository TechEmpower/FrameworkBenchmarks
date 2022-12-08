FROM mcr.microsoft.com/dotnet/sdk:7.0.100 AS build
WORKDIR /source
COPY src .
RUN dotnet publish -c Release -f net5.0 -o /app

FROM mcr.microsoft.com/dotnet/aspnet:7.0 AS runtime
# Full PGO
ENV DOTNET_TieredPGO 1 
ENV DOTNET_TC_QuickJitForLoops 1 
ENV DOTNET_ReadyToRun 0

ENV ASPNETCORE_URLS http://+:8080
WORKDIR /app
COPY --from=build /app .

EXPOSE 8080

ENTRYPOINT ["dotnet", "NancyBenchmark.dll"]
