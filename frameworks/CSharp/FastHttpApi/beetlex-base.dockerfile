FROM microsoft/dotnet:2.1-sdk-stretch AS build
WORKDIR /app
COPY PlatformBenchmarks .
RUN dotnet publish -c Release -o out

WORKDIR /app/out
ENTRYPOINT ["dotnet", "PlatformBenchmarks.dll"]
