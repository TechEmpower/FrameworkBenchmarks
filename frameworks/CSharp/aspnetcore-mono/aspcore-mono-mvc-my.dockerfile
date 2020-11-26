FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
WORKDIR /app
COPY Benchmarks .
RUN dotnet publish -c Release -o out

FROM mcr.microsoft.com/dotnet/runtime-deps:5.0 AS runtime
ENV ASPNETCORE_URLS http://+:8080
WORKDIR /app
COPY --from=build /app/out ./
COPY Benchmarks/appsettings.mysql.json ./appsettings.json

ENTRYPOINT ["./Benchmarks", "scenarios=MvcDbSingleQueryRaw,MvcDbMultiQueryRaw,MvcDbMultiUpdateRaw,MvcDbFortunesRaw"]