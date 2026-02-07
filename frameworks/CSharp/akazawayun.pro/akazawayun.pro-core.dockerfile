# 生成
FROM mcr.microsoft.com/dotnet/sdk:9.0 AS publish
WORKDIR /app
COPY src/AkazawaYun.Benchmark.Core .
RUN dotnet publish -c Release -o /app/publish

# 运行
FROM mcr.microsoft.com/dotnet/sdk:9.0 AS runtime
WORKDIR /app
COPY --from=publish /app/publish .

EXPOSE 8080
ENTRYPOINT ["dotnet", "AkazawaYun.Benchmark.Core.dll"]
