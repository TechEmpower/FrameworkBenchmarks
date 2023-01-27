FROM mcr.microsoft.com/dotnet/sdk:7.0.100 AS build
RUN apt-get update
RUN apt-get -yqq install clang zlib1g-dev
WORKDIR /app
COPY PlatformBenchmarks .
RUN dotnet publish -c Release -o out -p:PublishAot=true

FROM mcr.microsoft.com/dotnet/aspnet:7.0.0 AS runtime
ENV ASPNETCORE_URLS http://+:8080
ENV DOTNET_SYSTEM_NET_SOCKETS_INLINE_COMPLETIONS 1

WORKDIR /app
COPY --from=build /app/out ./
COPY Benchmarks/appsettings.json ./appsettings.json

EXPOSE 8080

ENTRYPOINT ["./PlatformBenchmarks"]
