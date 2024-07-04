FROM ubuntu:24.04

RUN apt update && apt install -y libgc-dev gcc build-essential curl git

ENV CHOOSENIM_NO_ANALYTICS 1
ENV CHOOSENIM_CHOOSE_VERSION 2.0.8
RUN curl https://nim-lang.org/choosenim/init.sh -sSf | sh -s -- -y
ENV PATH $PATH:/root/.nimble/bin

ADD ./ /happyx
WORKDIR /happyx
RUN nimble c -d:danger -d:httpbeast --threads:on -y -d:disableApiDoc techempower.nim

EXPOSE 5000

CMD ./techempower
