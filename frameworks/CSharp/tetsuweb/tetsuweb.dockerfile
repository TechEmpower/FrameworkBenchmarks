FROM mcr.microsoft.com/dotnet/core/sdk:5.0 AS build
WORKDIR /app
COPY Benchmark .
RUN dotnet publish -c Release -o out

FROM mcr.microsoft.com/dotnet/core/runtime:5.0
WORKDIR /app
COPY --from=build /app/out ./

ENTRYPOINT ["dotnet", "Benchmark.dll"]
