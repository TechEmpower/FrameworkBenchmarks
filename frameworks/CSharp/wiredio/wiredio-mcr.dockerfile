# Build
FROM mcr.microsoft.com/dotnet/sdk:10.0-alpine AS build
RUN apk add --no-cache clang build-base zlib-dev linux-headers
WORKDIR /src
COPY src/Platform/ ./Platform/
WORKDIR /src/Platform
RUN dotnet publish -c Release \
    -r linux-musl-x64 \
    --self-contained true \
    -p:PublishAot=true \
    -p:OptimizationPreference=Speed \
    -p:GarbageCollectionAdaptationMode=0 \
    -o /app/out

# Runtime (musl)
FROM mcr.microsoft.com/dotnet/runtime-deps:10.0-alpine
ENV URLS=http://+:8080
WORKDIR /app
COPY --from=build /app/out ./
RUN chmod +x ./Platform
EXPOSE 8080
ENTRYPOINT ["./Platform"]
