FROM mcr.microsoft.com/dotnet/core/sdk:3.1-alpine AS build
WORKDIR /source

# copy csproj and restore as distinct layers
COPY Benchmarks/*.csproj .
RUN dotnet restore -r linux-musl-x64

# copy and publish app and libraries
COPY Benchmarks/ .
RUN dotnet publish -c release -o /app -r linux-musl-x64 --self-contained true --no-restore /p:PublishTrimmed=true /p:PublishReadyToRun=true

# final stage/image
FROM mcr.microsoft.com/dotnet/core/runtime-deps:3.1-alpine
WORKDIR /app
COPY --from=build /app .

ENTRYPOINT ["./Benchmarks"]

EXPOSE 8080