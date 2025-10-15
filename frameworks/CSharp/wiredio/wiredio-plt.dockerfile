FROM mcr.microsoft.com/dotnet/sdk:9.0 AS build
RUN apt-get update && apt-get install -y --no-install-recommends clang lld zlib1g-dev && rm -rf /var/lib/apt/lists/*
WORKDIR /src
COPY src/Platform/ ./Platform/
WORKDIR /src/Platform
RUN dotnet publish -c Release \
    -r linux-x64 \
    --self-contained true \
    -p:PublishAot=true \
    -p:OptimizationPreference=Speed \
    -p:GarbageCollectionAdaptationMode=0 \
    -o /app/out

FROM mcr.microsoft.com/dotnet/runtime-deps:9.0
ENV URLS=http://+:8080
WORKDIR /app
COPY --from=build /app/out ./
RUN chmod +x ./Platform
EXPOSE 8080
ENTRYPOINT ["./Platform"]

