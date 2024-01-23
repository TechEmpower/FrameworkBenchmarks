FROM mcr.microsoft.com/dotnet/sdk:8.0.100 AS build
RUN apt-get update
RUN apt-get -yqq install clang zlib1g-dev libkrb5-dev libtinfo5

WORKDIR /app
COPY src .
RUN dotnet publish -c Release -o out

# Construct the actual image that will run
FROM mcr.microsoft.com/dotnet/aspnet:8.0.0 AS runtime

WORKDIR /app
COPY --from=build /app/out ./

EXPOSE 8080

ENTRYPOINT ["./appMpower"]