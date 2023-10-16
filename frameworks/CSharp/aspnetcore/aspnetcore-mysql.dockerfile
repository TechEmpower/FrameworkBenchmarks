FROM mcr.microsoft.com/dotnet/sdk:8.0.100-rc.2 AS build
WORKDIR /app
COPY src/Platform .
RUN dotnet publish -c Release -o out /p:DatabaseProvider=MySqlConnector

FROM mcr.microsoft.com/dotnet/aspnet:8.0.0-rc.2 AS runtime
ENV URLS http://+:8080

WORKDIR /app
COPY --from=build /app/out ./
COPY appsettings.mysql.json ./appsettings.json

EXPOSE 8080

ENTRYPOINT ["dotnet", "Platform.dll"]
