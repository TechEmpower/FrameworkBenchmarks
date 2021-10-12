FROM mcr.microsoft.com/dotnet/sdk:6.0.100-rc.1 AS build
WORKDIR /app
COPY PlatformBenchmarks .
RUN dotnet publish -c Release -o out

FROM mcr.microsoft.com/dotnet/aspnet:6.0.0-rc.1 AS runtime
ENV ASPNETCORE_URLS http://+:8080
ENV DOTNET_SYSTEM_NET_SOCKETS_INLINE_COMPLETIONS 1

# Full PGO
ENV DOTNET_TieredPGO 1 
ENV DOTNET_TC_QuickJitForLoops 1 
ENV DOTNET_ReadyToRun 0

WORKDIR /app
COPY --from=build /app/out ./
COPY Benchmarks/appsettings.json ./appsettings.json

EXPOSE 8080

ENTRYPOINT ["dotnet", "PlatformBenchmarks.dll"]
