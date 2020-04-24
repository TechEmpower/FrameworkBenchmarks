FROM mcr.microsoft.com/dotnet/core/sdk:3.1.101 AS build
RUN apt-get update
RUN apt-get -yqq install clang zlib1g-dev libkrb5-dev libtinfo5
WORKDIR /app
COPY PlatformBenchmarks .
RUN dotnet publish -c Release -o out -r linux-x64

FROM mcr.microsoft.com/dotnet/core/runtime-deps:3.1 AS runtime
WORKDIR /app
COPY --from=build /app/out ./

ENTRYPOINT ["./PlatformBenchmarks", "--server.urls=http://+:8080", "KestrelTransport=LinuxTransport"]
