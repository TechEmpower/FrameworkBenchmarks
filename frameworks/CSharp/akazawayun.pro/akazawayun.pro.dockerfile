# 此阶段用于生成项目
FROM mcr.microsoft.com/dotnet/sdk:9.0 AS publish
WORKDIR /app
COPY src .
RUN dotnet publish -c Release -o /app/publish

# 此阶段用于运行
FROM mcr.microsoft.com/dotnet/sdk:9.0 AS runtime
WORKDIR /app
COPY --from=publish /app/publish .

EXPOSE 2022
ENTRYPOINT ["dotnet", "AkazawaYun.FrameworkBenchmarks.dll"]