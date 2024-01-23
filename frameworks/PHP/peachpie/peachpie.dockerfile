FROM mcr.microsoft.com/dotnet/sdk:7.0.100 AS build
WORKDIR /app
COPY . .
RUN dotnet publish -c Release -o out Server

FROM mcr.microsoft.com/dotnet/aspnet:7.0 AS runtime
# Full PGO
ENV DOTNET_TieredPGO 1 
ENV DOTNET_TC_QuickJitForLoops 1 
ENV DOTNET_ReadyToRun 0

WORKDIR /app
COPY --from=build /app/out ./

EXPOSE 8080

ENTRYPOINT ["dotnet", "Server.dll", "--urls=http://*:8080"]
