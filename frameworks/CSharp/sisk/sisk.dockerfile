FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build
WORKDIR /source

# copy csproj and restore as distinct layers
COPY sisk/*.csproj .
RUN dotnet restore -r linux-musl-x64

# copy and publish app and libraries
COPY sisk/ .
RUN dotnet publish -c release -o /app -r linux-musl-x64

# final stage/image
FROM mcr.microsoft.com/dotnet/aspnet:8.0 AS runtime
WORKDIR /app
COPY --from=build /app .

ENTRYPOINT ["dotnet", "./sisk.dll"]

EXPOSE 8080