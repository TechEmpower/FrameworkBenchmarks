FROM microsoft/dotnet:2.2-sdk AS build
WORKDIR /app
COPY Benchmark .
RUN dotnet publish -c Release -o out

FROM mcr.microsoft.com/dotnet/core/runtime:2.2
WORKDIR /app
COPY --from=build /app/out ./

ENTRYPOINT ["dotnet", "Benchmark.dll"]
