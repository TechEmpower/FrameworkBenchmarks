FROM microsoft/dotnet:2.2-sdk AS build
WORKDIR /app
COPY Benchmarks .
RUN dotnet publish -c Release -o out

FROM mono:latest AS runtime
ENV ASPNETCORE_URLS http://+:8080
WORKDIR /app
COPY --from=build /app/out ./
COPY Benchmarks/appsettings.postgresql.json ./appsettings.json

ENTRYPOINT ["mono", "--server", "--gc=sgen", "--gc-params=mode=throughput", "Benchmarks.exe", "scenarios=MvcDbSingleQueryEf,MvcDbMultiQueryEf,MvcDbMultiUpdateEf,MvcDbFortunesEf"]