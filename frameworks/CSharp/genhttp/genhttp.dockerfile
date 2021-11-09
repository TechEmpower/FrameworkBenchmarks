FROM mcr.microsoft.com/dotnet/sdk:6.0-alpine AS build
WORKDIR /source

# copy csproj and restore as distinct layers
COPY Benchmarks/*.csproj .
RUN dotnet restore -r linux-musl-x64

# copy and publish app and libraries
COPY Benchmarks/ .
RUN dotnet publish -c release -o /app -r linux-musl-x64 --no-restore 

# final stage/image
FROM mcr.microsoft.com/dotnet/runtime-deps:6.0-alpine
WORKDIR /app
COPY --from=build /app .

ENTRYPOINT ["./Benchmarks"]

EXPOSE 8080