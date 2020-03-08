FROM mcr.microsoft.com/dotnet/core/sdk:2.1 AS build
WORKDIR /app
COPY PlatformBenchmarks .
RUN dotnet publish -c Release -o out

FROM mono:latest AS runtime
ENV ASPNETCORE_URLS http://+:8080
ENV KestrelTransport Libuv
WORKDIR /app
COPY --from=build /app/out ./
COPY Benchmarks/appsettings.mysql.json ./appsettings.json

ENTRYPOINT ["mono", "--server", "--gc=sgen", "--gc-params=mode=throughput", "PlatformBenchmarks.exe"]
