FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build
RUN apt-get update
RUN apt-get -yqq install clang zlib1g-dev
WORKDIR /app
COPY src/Platform .
RUN dotnet publish -c Release -o out /p:DatabaseProvider=Npgsql /p:PublishAot=true /p:OptimizationPreference=Speed /p:GarbageCollectionAdaptationMode=0

FROM mcr.microsoft.com/dotnet/aspnet:8.0 AS runtime
ENV URLS http://+:8080

WORKDIR /app
COPY --from=build /app/out ./
COPY appsettings.postgresql.json ./appsettings.json

EXPOSE 8080

ENTRYPOINT ["./Platform"]
