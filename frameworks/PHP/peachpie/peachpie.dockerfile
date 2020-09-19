FROM mcr.microsoft.com/dotnet/core/sdk:5.0 AS build
WORKDIR /app
COPY . .
RUN dotnet publish -c Release -o out Server

FROM mcr.microsoft.com/dotnet/core/aspnet:5.0 AS runtime
ENV COMPlus_ReadyToRun 0
WORKDIR /app
COPY --from=build /app/out ./

ENTRYPOINT ["dotnet", "Server.dll", "--urls=http://*:8080"]
