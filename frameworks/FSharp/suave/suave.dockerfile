FROM mcr.microsoft.com/dotnet/sdk:6.0
WORKDIR /app
COPY src/App .
RUN dotnet publish -c Release -o out
RUN pwd
RUN dir out/

EXPOSE 8080

ENTRYPOINT ["dotnet", "out/App.dll"]
