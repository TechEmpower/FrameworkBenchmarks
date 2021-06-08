FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
RUN apt-get update
RUN apt-get -yqq install clang zlib1g-dev libkrb5-dev libtinfo5

WORKDIR /app
COPY src .
RUN dotnet publish -c Release -o out -r linux-x64

FROM mcr.microsoft.com/dotnet/aspnet:5.0 AS runtime

RUN apt-get update
RUN apt-get install -y unixodbc odbc-postgresql

WORKDIR /etc/
COPY odbcinst.ini .

WORKDIR /app
COPY --from=build /app/out ./

EXPOSE 8080

ENTRYPOINT ["./appMpower"]