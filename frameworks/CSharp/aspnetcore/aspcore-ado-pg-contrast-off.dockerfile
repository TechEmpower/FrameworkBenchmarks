FROM mcr.microsoft.com/dotnet/sdk:6.0.100-rc.2 AS build
WORKDIR /app
COPY PlatformBenchmarks .
RUN dotnet publish -c Release -o out /p:DatabaseProvider=Npgsql

FROM mcr.microsoft.com/dotnet/aspnet:6.0.0-rc.2 AS runtime
ENV ASPNETCORE_URLS http://+:8080

# Full PGO
ENV DOTNET_TieredPGO 1 
ENV DOTNET_TC_QuickJitForLoops 1 
ENV DOTNET_ReadyToRun 0

WORKDIR /app
COPY --from=build /app/out ./
COPY PlatformBenchmarks/appsettings.postgresql.json ./appsettings.json

EXPOSE 8080

# Start Contrast Additions
RUN apt-get update && apt-get install unzip

COPY Contrast.NET.Core.zip Contrast.NET.Core.zip
COPY contrast_security.yaml /etc/contrast/contrast_security.yaml

ENV CONTRAST__ASSESS__ENABLE=false
ENV CONTRAST__PROTECT__ENABLE=false

ENV CORECLR_PROFILER_PATH_64="/app/Contrast.NET.Core/runtimes/linux-x64/native/ContrastProfiler.so"
ENV CORECLR_PROFILER="{8B2CE134-0948-48CA-A4B2-80DDAD9F5791}"
ENV CORECLR_ENABLE_PROFILING=1
ENV CONTRAST_CONFIG_PATH=/etc/contrast/contrast_security.yaml

RUN unzip Contrast.NET.Core.zip -d Contrast.NET.Core
# End Contrast Additions

ENTRYPOINT ["dotnet", "PlatformBenchmarks.dll"]
