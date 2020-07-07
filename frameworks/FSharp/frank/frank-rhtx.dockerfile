FROM mcr.microsoft.com/dotnet/core/sdk:3.1.300 AS build
WORKDIR /app
COPY src/App .
RUN dotnet publish -c Release -o out

FROM mcr.microsoft.com/dotnet/core/aspnet:3.1.4 AS runtime
ENV ASPNETCORE_URLS http://+:8080
WORKDIR /app
COPY --from=build /app/out ./

ENTRYPOINT ["dotnet", "App.dll", "KestrelTransport=LinuxTransport"]
