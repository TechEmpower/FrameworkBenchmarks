FROM microsoft/dotnet:2.1-sdk-stretch AS build
WORKDIR /app
COPY Benchmarks .
RUN dotnet publish -c Release -o out
COPY Benchmarks/appsettings.ravendb.json ./out/appsettings.json

FROM microsoft/dotnet:2.1-aspnetcore-runtime AS runtime
ENV ASPNETCORE_URLS http://+:8080
ENV COMPlus_ReadyToRun 0
WORKDIR /app
COPY --from=build /app/out ./

ENTRYPOINT ["dotnet", "Benchmarks.dll", "scenarios=DbSingleQueryRaven,DbMultiQueryRaven,DbMultiUpdateRaven,DbFortunesRaven"]