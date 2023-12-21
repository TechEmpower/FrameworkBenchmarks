FROM ubuntu:22.04 AS build

# prevent timezone dialogue
ENV DEBIAN_FRONTEND=noninteractive

RUN apt update && \
    apt upgrade -y
RUN apt install -y --fix-missing \
        gcc \
        xz-utils \
        ca-certificates \
        curl \
        git

ARG VERSION="2.0.0"
WORKDIR /root
RUN curl https://nim-lang.org/choosenim/init.sh -o init.sh
RUN sh init.sh -y
RUN rm -f init.sh
ENV PATH $PATH:/root/.nimble/bin
RUN choosenim ${VERSION}

ENV PATH $PATH:/root/.nimble/bin

ADD ./ /basolato
WORKDIR /basolato

RUN nimble install -y
RUN ducere build -p:8080 -w:4
RUN chmod 111 main
RUN chmod 111 startServer.sh


FROM ubuntu:22.04 AS runtime

# prevent timezone dialogue
ENV DEBIAN_FRONTEND=noninteractive

RUN apt update && \
    apt upgrade -y
RUN apt install -y --fix-missing \
        xz-utils \
        ca-certificates \
        libpq-dev

WORKDIR /basolato
COPY --from=build /basolato/main .
COPY --from=build /basolato/startServer.sh .

EXPOSE 8080

CMD ./startServer.sh
