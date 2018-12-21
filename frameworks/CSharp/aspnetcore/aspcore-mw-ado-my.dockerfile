FROM microsoft/dotnet:2.2-sdk AS build
WORKDIR /app
COPY Benchmarks .
RUN dotnet publish -c Release -o out

FROM microsoft/dotnet:2.2-aspnetcore-runtime AS runtime
ENV ASPNETCORE_URLS http://+:8080
ENV COMPlus_ReadyToRun 0
WORKDIR /app
COPY --from=build /app/out ./
COPY Benchmarks/appsettings.mysql.json ./appsettings.json

ENTRYPOINT ["dotnet", "Benchmarks.dll", "scenarios=DbSingleQueryRaw,DbMultiQueryRaw,DbMultiUpdateRaw,DbFortunesRaw"]