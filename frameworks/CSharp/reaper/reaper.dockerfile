FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build
WORKDIR /src
COPY src .
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       clang zlib1g-dev
WORKDIR "/src/Benchmark"
RUN dotnet publish "Benchmark.csproj" -c Release -o /app/publish

FROM mcr.microsoft.com/dotnet/aspnet:8.0 AS final
WORKDIR /app
EXPOSE 8080
COPY --from=build /app/publish .
ENTRYPOINT ["./Benchmark"]
