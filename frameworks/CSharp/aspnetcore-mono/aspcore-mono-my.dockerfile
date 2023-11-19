FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build
WORKDIR /app
COPY PlatformBenchmarks .
RUN dotnet publish -c Release -o out /p:IsDatabase=true

FROM mcr.microsoft.com/dotnet/runtime-deps:8.0 AS runtime
ENV ASPNETCORE_URLS http://+:8080
WORKDIR /app
COPY --from=build /app/out ./
COPY Benchmarks/appsettings.mysql.json ./appsettings.json

EXPOSE 8080

ENTRYPOINT ["./PlatformBenchmarks"]
