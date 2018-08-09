FROM microsoft/dotnet:2.1-sdk-stretch
WORKDIR /app
COPY src/App .
RUN dotnet publish -c Release -o out
RUN pwd
RUN dir out/
ENTRYPOINT ["dotnet", "out/App.dll"]
