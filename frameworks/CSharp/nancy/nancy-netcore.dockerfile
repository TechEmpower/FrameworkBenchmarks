FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
WORKDIR /source
COPY src .
RUN dotnet publish -c Release -f net5.0 -o /app

FROM mcr.microsoft.com/dotnet/aspnet:5.0 AS runtime
ENV ASPNETCORE_URLS http://+:8080
WORKDIR /app
COPY --from=build /app .

EXPOSE 8080

ENTRYPOINT ["dotnet", "NancyBenchmark.dll"]
