FROM mcr.microsoft.com/dotnet/core/sdk:3.1.101 AS build
WORKDIR /app
COPY PlatformBenchmarks .
RUN dotnet publish -c Release -o out

FROM mcr.microsoft.com/dotnet/core/aspnet:3.1.2 AS runtime
WORKDIR /app
COPY --from=build /app/out ./
ENTRYPOINT ["dotnet", "PlatformBenchmarks.dll"]
