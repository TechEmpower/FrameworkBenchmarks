FROM mcr.microsoft.com/dotnet/sdk:10.0 AS build
RUN apt-get update
RUN apt-get -yqq install clang zlib1g-dev
RUN apt-get update

WORKDIR /app
COPY src .
RUN dotnet publish -c Release -o out /p:AOTEXE=true /p:Database=mysql /p:Driver=ado

# Construct the actual image that will run
FROM mcr.microsoft.com/dotnet/aspnet:10.0.1 AS runtime

RUN apt-get update
RUN apt-get install -y unixodbc-dev unixodbc wget curl
RUN apt-get update

# Full PGO
ENV DOTNET_TieredPGO 1 
ENV DOTNET_TC_QuickJitForLoops 1 
ENV DOTNET_ReadyToRun 0

ENV ASPNETCORE_URLS http://+:8080
WORKDIR /app
COPY --from=build /app/out ./

#TEST: ./tfb --test appmpower-odbc-my  --type db

EXPOSE 8080

ENTRYPOINT ["./appMpower"]