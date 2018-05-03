FROM ubuntu:16.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq  > /dev/null
RUN apt-get install -yqq git unzip php7.2 php7.2-common php7.2-cli php7.2-dev php7.2-mbstring composer curl build-essential > /dev/null

# An extension is required!
# We deal with concurrencies over 1k, which stream_select doesn't support.

ADD ./install-ev.sh /install-ev.sh
RUN /install-ev.sh > /dev/null

ADD ./ /amp
WORKDIR /amp

COPY deploy/conf/* /etc/php/7.2/cli/conf.d/

RUN composer install --quiet

CMD php /amp/vendor/bin/cluster -s /amp/server.php
