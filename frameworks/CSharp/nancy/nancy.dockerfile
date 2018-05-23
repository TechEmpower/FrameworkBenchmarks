FROM microsoft/dotnet:2.1-sdk-stretch AS build
WORKDIR /app
COPY src .
RUN dotnet publish -c Release -f net471 -o out

FROM mono:5.12.0.226 AS runtime
WORKDIR /app
COPY --from=build /app/out ./
ENV ASPNETCORE_URLS http://+:8080

ENTRYPOINT ["mono", "--server", "--gc=sgen", "--gc-params=mode=throughput", "NancyBenchmark.exe"]
