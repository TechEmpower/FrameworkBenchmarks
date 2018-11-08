FROM microsoft/dotnet:2.1-sdk-stretch AS build
WORKDIR /app
COPY Benchmarks .
RUN dotnet publish -c Release -o out

FROM microsoft/dotnet:2.1-runtime-nanoserver
WORKDIR /app/out
ENTRYPOINT ["dotnet", "Benchmarks.dll"]
