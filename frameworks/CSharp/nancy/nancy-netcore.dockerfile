FROM mcr.microsoft.com/dotnet/core/sdk:3.1.101 AS build
WORKDIR /app
COPY src .
RUN dotnet publish -c Release -f netcoreapp3.1 -o out

FROM mcr.microsoft.com/dotnet/core/aspnet:3.1.2 AS runtime
ENV ASPNETCORE_URLS http://+:8080
ENV COMPlus_ReadyToRun 0
WORKDIR /app
COPY --from=build /app/out ./

ENTRYPOINT ["dotnet", "NancyBenchmark.dll"]
