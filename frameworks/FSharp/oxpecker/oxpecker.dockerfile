FROM mcr.microsoft.com/dotnet/sdk:9.0 AS build
WORKDIR /app
COPY src/App .
RUN dotnet publish -c Release -o out

FROM mcr.microsoft.com/dotnet/aspnet:9.0 AS runtime

ENV DOTNET_GCDynamicAdaptationMode=0
ENV DOTNET_ReadyToRun 0
ENV DOTNET_HillClimbing_Disable=1
ENV ASPNETCORE_hostBuilder__reloadConfigOnChange false

ENV URLS http://+:8080

WORKDIR /app
COPY --from=build /app/out ./

EXPOSE 8080

ENTRYPOINT ["dotnet", "App.dll"]