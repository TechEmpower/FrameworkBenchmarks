FROM microsoft/dotnet:2.2-sdk-stretch

COPY . /wwwroot
WORKDIR /wwwroot

RUN apt update -y \
    && apt install -y ca-certificates apt-transport-https \
    && wget -q https://packages.sury.org/php/apt.gpg -O- | apt-key add - \
    && echo "deb https://packages.sury.org/php/ stretch main" | tee /etc/apt/sources.list.d/php.list \
    && apt-get update -y \
    && apt-get install -y nginx git unzip php7.3 php7.3-common php7.3-cli php7.3-fpm php7.3-mysql

RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer \
    && composer update --no-dev

RUN dotnet publish -c Release -o /wwwroot /wwwroot/deploy/peachpie/Server

EXPOSE 8080/tcp
ENV ASPNETCORE_URLS="http://+:8080"

ENTRYPOINT ["dotnet", "Server.dll"]
