FROM mcr.microsoft.com/dotnet/core/sdk:2.1
WORKDIR /app
COPY src/App .
RUN dotnet publish -c Release -o out
RUN pwd
RUN dir out/
ENTRYPOINT ["dotnet", "out/App.dll"]
