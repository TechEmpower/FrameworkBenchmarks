FROM mcr.microsoft.com/dotnet/sdk:10.0-alpine AS build
WORKDIR /source

# copy csproj and restore as distinct layers
COPY sisk-cadente/*.csproj .
RUN dotnet restore -r linux-musl-x64

# copy and publish app and libraries
COPY sisk-cadente/ .
RUN dotnet publish -c release -o /app -r linux-musl-x64 --no-restore --self-contained

# final stage/image
FROM mcr.microsoft.com/dotnet/runtime-deps:10.0-alpine

ENV DOTNET_GCDynamicAdaptationMode=0
ENV DOTNET_ReadyToRun=0
ENV DOTNET_HillClimbing_Disable=1

WORKDIR /app
COPY --from=build /app .

ENTRYPOINT ["./sisk"]

EXPOSE 8080