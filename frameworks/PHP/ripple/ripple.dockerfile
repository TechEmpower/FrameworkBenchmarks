FROM php:8.3-cli

RUN apt-get update -yqq >> /dev/null
RUN apt-get install -y libevent-dev \
    libffi-dev \
    libssl-dev \
    pkg-config \
    build-essential \
    unzip >> /dev/null

RUN docker-php-ext-install pdo_mysql \
    ffi \
    opcache \
    posix \
    pcntl \
    sockets >> /dev/null

RUN pecl install ev >> /dev/null

RUN docker-php-ext-enable posix pcntl sockets ev

COPY --from=composer --link /usr/bin/composer /usr/local/bin/composer

# Initialize
WORKDIR /ripple
COPY --link . .

# Configure
RUN composer install

# Start
EXPOSE 8080
ENTRYPOINT ["php","server.php"]
