FROM mcr.microsoft.com/dotnet/core/sdk:3.0 AS build
WORKDIR /app
COPY PlatformBenchmarks .
RUN dotnet publish -c Release -o out

FROM mcr.microsoft.com/dotnet/core/aspnet:3.0 AS runtime
ENV COMPlus_ReadyToRun 0
WORKDIR /app
COPY --from=build /app/out ./

EXPOSE 8080

ENTRYPOINT ["dotnet", "PlatformBenchmarks.dll"]
