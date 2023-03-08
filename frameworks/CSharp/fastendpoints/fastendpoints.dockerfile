FROM mcr.microsoft.com/dotnet/sdk:7.0 AS build
WORKDIR /app
COPY Benchmarks .
RUN dotnet publish -c Release -o out

FROM mcr.microsoft.com/dotnet/aspnet:7.0 AS runtime
WORKDIR /app
COPY --from=build /app/out ./

ENV DOTNET_TieredPGO 1 
ENV DOTNET_TC_QuickJitForLoops 1 
ENV DOTNET_ReadyToRun 0

EXPOSE 8080

ENTRYPOINT ["dotnet", "Benchmarks.dll"]