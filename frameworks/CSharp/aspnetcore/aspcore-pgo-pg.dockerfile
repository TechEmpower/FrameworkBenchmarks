FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
WORKDIR /app
COPY PlatformBenchmarks .
RUN dotnet publish -c Release -o out /p:IsDatabase=true

FROM mcr.microsoft.com/dotnet/aspnet:5.0 AS runtime
ENV ASPNETCORE_URLS http://+:8080
WORKDIR /app
COPY --from=build /app/out ./
COPY PlatformBenchmarks/appsettings.postgresql.json ./appsettings.json

# Switch off AoT code in libs to allow for greater instrumentation
ENV COMPlus_ReadyToRun 0
# Move methods with loops to Tier0 rather than Tier1 by default for greater instrumentation
ENV COMPlus_TC_QuickJitForLoops 1
# Switch on Profile Guided Optimization instrumentation at Tier0 
ENV COMPlus_TieredPGO 1

ENTRYPOINT ["dotnet", "PlatformBenchmarks.dll"]
