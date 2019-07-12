FROM mcr.microsoft.com/dotnet/core/sdk:2.2 AS build
WORKDIR /app
COPY . .
RUN dotnet publish -c Release -o ../out Server

FROM mcr.microsoft.com/dotnet/core/aspnet:2.2 AS runtime
ENV COMPlus_ReadyToRun 0
WORKDIR /app
COPY --from=build /app/out ./

ENTRYPOINT ["dotnet", "Server.dll"]
