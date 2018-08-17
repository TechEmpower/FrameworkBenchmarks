FROM gcc:latest

RUN apt update && apt install -y libgc-dev

ENV CHOOSENIM_NO_ANALYTICS 1
ENV CHOOSENIM_CHOOSE_VERSION #0c683d28bbd5
RUN curl https://nim-lang.org/choosenim/init.sh -sSf | sh -s -- -y
ENV PATH $PATH:/root/.nimble/bin

ADD ./ /jester
WORKDIR /jester
RUN nimble install -y httpbeast@#v0.2.0
RUN nimble c -d:release --threads:on -y techempower.nim

CMD ./techempower
