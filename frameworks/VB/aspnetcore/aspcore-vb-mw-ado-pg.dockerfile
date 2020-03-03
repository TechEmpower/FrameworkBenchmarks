FROM mcr.microsoft.com/dotnet/core/sdk:2.1 AS build
WORKDIR /app
COPY Benchmarks .
RUN dotnet publish -c Release -o out
COPY Benchmarks/appsettings.postgresql.json ./out/appsettings.json

FROM mcr.microsoft.com/dotnet/core/aspnet:2.1 AS runtime
ENV ASPNETCORE_URLS http://+:8080
ENV COMPlus_ReadyToRun 0
WORKDIR /app
COPY --from=build /app/out ./

ENTRYPOINT ["dotnet", "Benchmarks.dll"]