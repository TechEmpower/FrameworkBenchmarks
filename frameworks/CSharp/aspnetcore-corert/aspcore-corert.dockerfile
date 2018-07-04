FROM microsoft/dotnet:2.1-sdk-stretch AS build
RUN echo "deb http://llvm.org/apt/trusty/ llvm-toolchain-trusty-3.9 main" | tee /etc/apt/sources.list.d/llvm.list
RUN wget -O - http://llvm.org/apt/llvm-snapshot.gpg.key | apt-key add -
RUN apt-get update
RUN apt-get -yqq install cmake clang-3.9 libicu57 libunwind8 uuid-dev libcurl4-openssl-dev zlib1g-dev libkrb5-dev
WORKDIR /app
COPY PlatformBenchmarks .
RUN dotnet publish -c Release -o out -r linux-x64

FROM microsoft/dotnet:2.1-aspnetcore-runtime AS runtime
WORKDIR /app
COPY --from=build /app/out ./

ENTRYPOINT ["./PlatformBenchmarks", "--server.urls=http://+:8080"]
