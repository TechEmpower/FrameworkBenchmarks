# 生成
FROM mcr.microsoft.com/dotnet/sdk:9.0 AS publish
RUN apt-get update
RUN apt-get -yqq install clang zlib1g-dev
WORKDIR /app
COPY src/AkazawaYun.Benchmark.Core .
RUN dotnet publish -c Release -o /app/publish /p:PublishAot=true /p:OptimizationPreference=Speed /p:GarbageCollectionAdaptationMode=0

# 运行
FROM mcr.microsoft.com/dotnet/sdk:9.0 AS runtime
WORKDIR /app
COPY --from=publish /app/publish .

EXPOSE 8080
ENTRYPOINT ["./AkazawaYun.Benchmark.Core"]
