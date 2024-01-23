FROM ubuntu:18.04

RUN apt update && apt install -y libgc-dev gcc build-essential curl git

ENV CHOOSENIM_NO_ANALYTICS 1
ENV CHOOSENIM_CHOOSE_VERSION 1.4.0
RUN curl https://nim-lang.org/choosenim/init.sh -sSf | sh -s -- -y
ENV PATH $PATH:/root/.nimble/bin

ADD ./ /httpbeast
WORKDIR /httpbeast
RUN nimble c -d:danger --threads:on -y techempower.nim

EXPOSE 8080

CMD ./techempower
