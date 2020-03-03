FROM mcr.microsoft.com/dotnet/core/sdk:3.1-alpine AS build
WORKDIR /app

# copy csproj and restore as distinct layers
COPY Benchmarks/*.csproj ./Benchmarks/
WORKDIR /app/Benchmarks
RUN dotnet restore

# copy and build app and libraries
WORKDIR /app/
COPY Benchmarks/. ./Benchmarks/
WORKDIR /app/Benchmarks
RUN dotnet publish -c Release -r linux-musl-x64 -o out --self-contained true /p:PublishTrimmed=true

FROM mcr.microsoft.com/dotnet/core/runtime-deps:3.1-alpine AS runtime
ENV DOCKER_FLAVOR=linux
WORKDIR /app
COPY --from=build /app/Benchmarks/out ./
ENTRYPOINT ["./Benchmarks"]

EXPOSE 8080