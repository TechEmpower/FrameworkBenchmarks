FROM ubuntu:19.10

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq
RUN apt-get install -y nginx-light

ADD ./ ./

CMD nginx -c /nginx.conf
