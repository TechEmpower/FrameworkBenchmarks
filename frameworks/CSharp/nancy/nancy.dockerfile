FROM mcr.microsoft.com/dotnet/core/sdk:3.1.101 AS build
WORKDIR /app
COPY src .
RUN dotnet publish -c Release -f net471 -o out

FROM mono:6.8 AS runtime
WORKDIR /app
COPY --from=build /app/out ./
ENV ASPNETCORE_URLS http://+:8080

ENTRYPOINT ["mono", "--server", "--gc=sgen", "--gc-params=mode=throughput", "NancyBenchmark.exe"]
