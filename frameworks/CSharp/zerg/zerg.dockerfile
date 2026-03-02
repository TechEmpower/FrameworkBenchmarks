# Build
FROM mcr.microsoft.com/dotnet/sdk:10.0-alpine AS build
RUN apk add --no-cache clang build-base linux-headers liburing-dev
WORKDIR /src
COPY silverlight/ ./silverlight/

# Build native shim
WORKDIR /src/silverlight
RUN clang -O2 -fPIC -shared uringshim.c -o liburingshim.so -luring

# Publish app
RUN dotnet publish -c Release -o /app/out
RUN cp /src/silverlight/liburingshim.so /app/out/runtimes/linux-musl-x64/native/liburingshim.so

# Runtime
FROM mcr.microsoft.com/dotnet/aspnet:10.0-alpine
RUN apk add --no-cache liburing
ENV URLS=http://+:8080 \
    LD_LIBRARY_PATH=/app
WORKDIR /app
COPY --from=build /app/out ./
EXPOSE 8080
ENTRYPOINT ["dotnet", "silverlight.dll"]
