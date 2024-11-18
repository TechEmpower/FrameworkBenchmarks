FROM mcr.microsoft.com/dotnet/sdk:9.0-alpine AS build
WORKDIR /app
COPY Benchmarks .
RUN dotnet publish -c release -o /app -r linux-musl-x64 --no-restore --self-contained

FROM mcr.microsoft.com/dotnet/runtime:9.0 AS runtime

ENV DOTNET_TieredPGO 1 
ENV DOTNET_TC_QuickJitForLoops 1 
ENV DOTNET_ReadyToRun 0
ENV DOTNET_GCDynamicAdaptationMode 0
ENV DOTNET_HillClimbing_Disable 1
ENV DOTNET_SYSTEM_NET_SOCKETS_INLINE_COMPLETIONS 1

WORKDIR /app
COPY --from=build /app/out ./

EXPOSE 8080

ENTRYPOINT ["./Benchmarks"]
