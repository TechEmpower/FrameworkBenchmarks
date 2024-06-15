FROM mcr.microsoft.com/dotnet/sdk:8.0.204 AS build
WORKDIR /app
COPY src/App .
RUN dotnet publish -c Release -o out

FROM mcr.microsoft.com/dotnet/aspnet:8.0 AS runtime
# Full PGO
ENV DOTNET_TieredPGO 1
ENV DOTNET_TC_QuickJitForLoops 1
ENV DOTNET_ReadyToRun 0

ENV ASPNETCORE_URLS http://+:8080
WORKDIR /app
COPY --from=build /app/out ./

EXPOSE 8080

ENTRYPOINT ["dotnet", "App.dll","simple"]
