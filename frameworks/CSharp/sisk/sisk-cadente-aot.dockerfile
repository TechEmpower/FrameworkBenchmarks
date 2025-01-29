FROM mcr.microsoft.com/dotnet/sdk:9.0 AS build
WORKDIR /source

# copy csproj and restore as distinct layers
COPY sisk-cadente/*.csproj .
RUN dotnet restore -r linux-musl-x64

# copy and publish app and libraries
COPY sisk-cadente/ .
RUN dotnet publish -c release -o /app -r linux-musl-x64 /p:PublishAot=true /p:OptimizationPreference=Speed /p:GarbageCollectionAdaptationMode=0

# final stage/image
FROM mcr.microsoft.com/dotnet/aspnet:9.0 AS runtime
WORKDIR /app
COPY --from=build /app .

ENTRYPOINT ["./sisk"]

EXPOSE 8080