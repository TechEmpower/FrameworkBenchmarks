FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
WORKDIR /app
COPY PlatformBenchmarks .
RUN dotnet publish -c Release -o out

FROM mcr.microsoft.com/dotnet/aspnet:5.0 AS runtime
ENV ASPNETCORE_URLS http://+:8080
ENV DOTNET_SYSTEM_NET_SOCKETS_INLINE_COMPLETIONS 1
WORKDIR /app
COPY --from=build /app/out ./
COPY Benchmarks/appsettings.json ./appsettings.json

# Switch off AoT code in libs to allow for greater instrumentation
ENV COMPlus_ReadyToRun 0
# Move methods with loops to Tier0 rather than Tier1 by default for greater instrumentation
ENV COMPlus_TC_QuickJitForLoops 1
# Switch on Profile Guided Optimization instrumentation at Tier0 
ENV COMPlus_TieredPGO 1

ENTRYPOINT ["dotnet", "PlatformBenchmarks.dll"]
