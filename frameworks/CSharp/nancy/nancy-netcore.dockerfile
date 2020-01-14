FROM mcr.microsoft.com/dotnet/core/sdk:2.1 AS build
WORKDIR /app
COPY src .
RUN dotnet publish -c Release -f netcoreapp2.1 -o out

FROM mcr.microsoft.com/dotnet/core/aspnet:2.1 AS runtime
ENV ASPNETCORE_URLS http://+:8080
ENV COMPlus_ReadyToRun 0
WORKDIR /app
COPY --from=build /app/out ./

ENTRYPOINT ["dotnet", "NancyBenchmark.dll"]
