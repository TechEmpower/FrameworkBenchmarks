FROM microsoft/dotnet:2.1-sdk-stretch AS build
WORKDIR /app
COPY . .
RUN dotnet publish -c Release -o ../out Server

FROM microsoft/dotnet:2.1-aspnetcore-runtime AS runtime
ENV COMPlus_ReadyToRun 0
WORKDIR /app
COPY --from=build /app/out ./

ENTRYPOINT ["dotnet", "Server.dll"]
