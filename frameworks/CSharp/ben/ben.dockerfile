FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
WORKDIR /app
COPY Benchmarks .
RUN dotnet publish -c Release -o out

FROM mcr.microsoft.com/dotnet/aspnet:5.0 AS runtime
ENV DOTNET_SYSTEM_NET_SOCKETS_INLINE_COMPLETIONS 1
WORKDIR /app
COPY --from=build /app/out ./
ENV DB_CONNECTION Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=256;NoResetOnClose=true;Enlist=false;Max Auto Prepare=4;Multiplexing=true;Write Coalescing Delay Us=500;Write Coalescing Buffer Threshold Bytes=5500

ENTRYPOINT ["dotnet", "Benchmarks.dll"]
