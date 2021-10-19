FROM mcr.microsoft.com/dotnet/sdk:6.0.100-rc.2 AS build
RUN apt-get update
RUN apt-get -yqq install clang zlib1g-dev libkrb5-dev libtinfo5
WORKDIR /app
COPY PlatformBenchmarks .
RUN dotnet publish -c Release -o out -r linux-x64

FROM mcr.microsoft.com/dotnet/aspnet:6.0.0-rc.2 AS runtime
ENV ASPNETCORE_URLS http://+:8080
ENV DOTNET_SYSTEM_NET_SOCKETS_INLINE_COMPLETIONS 1
WORKDIR /app
COPY --from=build /app/out ./
COPY Benchmarks/appsettings.json ./appsettings.json

EXPOSE 8080

ENTRYPOINT ["./PlatformBenchmarks"]
