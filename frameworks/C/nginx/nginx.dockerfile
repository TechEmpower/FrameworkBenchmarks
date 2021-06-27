FROM ubuntu:20.10

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq
RUN apt-get install -y nginx-light

ADD ./ ./

EXPOSE 8080

CMD nginx -c /nginx.conf
