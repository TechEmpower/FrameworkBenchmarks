FROM mcr.microsoft.com/dotnet/sdk:5.0
WORKDIR /app
COPY src/App .
RUN dotnet publish -c Release -o out
RUN pwd
RUN dir out/
ENTRYPOINT ["dotnet", "out/App.dll"]
