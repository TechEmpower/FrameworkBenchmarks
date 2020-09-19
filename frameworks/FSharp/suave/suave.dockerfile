FROM mcr.microsoft.com/dotnet/core/sdk:5.0
WORKDIR /app
COPY src/App .
RUN dotnet publish -c Release -o out
RUN pwd
RUN dir out/
ENTRYPOINT ["dotnet", "out/App.dll"]
