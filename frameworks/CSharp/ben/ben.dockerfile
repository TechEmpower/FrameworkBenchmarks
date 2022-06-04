FROM mcr.microsoft.com/dotnet/sdk:6.0 AS build
WORKDIR /app
COPY Benchmarks .
RUN dotnet publish -c Release -o out

FROM mcr.microsoft.com/dotnet/aspnet:6.0 AS runtime

WORKDIR /app
COPY --from=build /app/out ./

ENV COMPlus_ReadyToRun 0
ENV COMPlus_TC_QuickJitForLoops 1
ENV COMPlus_TieredPGO 1
ENV DB_CONNECTION Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=256;NoResetOnClose=true;Enlist=false;Max Auto Prepare=4;Multiplexing=true;Write Coalescing Delay Us=500;Write Coalescing Buffer Threshold Bytes=1000

EXPOSE 8080

ENTRYPOINT ["dotnet", "Benchmarks.dll"]
