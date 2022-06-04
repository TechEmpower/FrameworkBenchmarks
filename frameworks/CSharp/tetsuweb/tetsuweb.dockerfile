FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
WORKDIR /app
COPY Benchmark .
RUN dotnet publish -c Release -o out

FROM mcr.microsoft.com/dotnet/runtime:5.0
WORKDIR /app
COPY --from=build /app/out ./

EXPOSE 8080

ENTRYPOINT ["dotnet", "Benchmark.dll"]
