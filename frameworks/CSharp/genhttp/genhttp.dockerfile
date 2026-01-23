FROM mcr.microsoft.com/dotnet/sdk:10.0-alpine AS build
WORKDIR /source

ENV GENHTTP_ENGINE_NAME=Internal

# copy csproj and restore as distinct layers
COPY Benchmarks/*.csproj .
RUN dotnet restore -r linux-musl-x64

# copy and publish app and libraries
COPY Benchmarks/ .
RUN dotnet publish -c release -o /app -r linux-musl-x64 --no-restore --self-contained

# final stage/image
FROM mcr.microsoft.com/dotnet/runtime-deps:10.0-alpine

ENV DOTNET_GCDynamicAdaptationMode=0 \
    DOTNET_EnableDiagnostics=0 \
    DOTNET_TieredPGO=1 \
    DOTNET_TC_QuickJitForLoops=0 \
    DOTNET_ReadyToRun=0 \
    DOTNET_TieredCompilation=0 \
    DOTNET_gcServer=1 \
    DB_CONNECTION="Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;SSL Mode=Disable;Maximum Pool Size=18;Enlist=false;Max Auto Prepare=4;Multiplexing=true;Write Coalescing Buffer Threshold Bytes=1000;"    

WORKDIR /app
COPY --from=build /app .

ENTRYPOINT ["./Benchmarks"]

EXPOSE 8080
