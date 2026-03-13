# Build
FROM mcr.microsoft.com/dotnet/sdk:10.0-alpine AS build
RUN apk add --no-cache clang build-base zlib-dev linux-headers liburing-dev
WORKDIR /src
COPY silverlight/ ./silverlight/

# Build native shim
WORKDIR /src/silverlight
RUN clang -O2 -fPIC -shared uringshim.c -o liburingshim.so -luring

# Publish AOT app
RUN dotnet publish -c Release \
    -r linux-musl-x64 \
    --self-contained true \
    -p:PublishAot=true \
    -p:OptimizationPreference=Speed \
    -p:GarbageCollectionAdaptationMode=0 \
    -o /app/out
RUN cp /src/silverlight/liburingshim.so /app/out/runtimes/linux-musl-x64/native/liburingshim.so 2>/dev/null; \
    cp /src/silverlight/liburingshim.so /app/out/liburingshim.so

# Runtime (musl)
FROM mcr.microsoft.com/dotnet/runtime-deps:10.0-alpine
RUN apk add --no-cache liburing
ENV URLS=http://+:8080 \
    LD_LIBRARY_PATH=/app
WORKDIR /app
COPY --from=build /app/out ./
RUN chmod +x ./silverlight
EXPOSE 8080
ENTRYPOINT ["./silverlight"]
