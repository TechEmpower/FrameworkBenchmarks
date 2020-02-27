FROM mcr.microsoft.com/dotnet/core/sdk:3.1.101 AS build
WORKDIR /source
COPY src .
RUN dotnet publish -c Release -f netcoreapp3.1 -o /app

FROM mcr.microsoft.com/dotnet/core/aspnet:3.1.2 AS runtime
ENV ASPNETCORE_URLS http://+:8080
WORKDIR /app
COPY --from=build /app .

ENTRYPOINT ["dotnet", "NancyBenchmark.dll"]
