FROM microsoft/dotnet:2.1-sdk-stretch AS build
WORKDIR /app
COPY Benchmarks .
RUN dotnet publish -c Release -o out
COPY Benchmarks/appsettings.postgresql.json ./out/appsettings.json

FROM mono:5.12.0.226 AS runtime
ENV ASPNETCORE_URLS http://+:8080
WORKDIR /app
COPY --from=build /app/out ./

ENTRYPOINT ["mono", "--server", "--gc=sgen", "--gc-params=mode=throughput", "Benchmarks.exe", "scenarios=MvcDbSingleQueryRaw,MvcDbMultiQueryRaw,MvcDbMultiUpdateRaw,MvcDbFortunesRaw"]