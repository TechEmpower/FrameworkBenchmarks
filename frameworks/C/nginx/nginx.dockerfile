FROM ubuntu:18.10

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository -y ppa:ondrej/nginx-mainline
#RUN add-apt-repository -y ppa:nginx/development
RUN apt-get update -yqq
RUN apt-get install -y nginx-light

ADD ./ ./

CMD nginx -c /nginx.conf