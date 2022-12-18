FROM debian:stretch-slim AS debian

ARG DEBIAN_FRONTEND=noninteractive
ARG TERM=linux

RUN echo 'APT::Get::Install-Recommends "false";' > /etc/apt/apt.conf.d/00-general \
    && echo 'APT::Get::Install-Suggests "false";' >> /etc/apt/apt.conf.d/00-general \
    && echo 'APT::Get::Assume-Yes "true";' >> /etc/apt/apt.conf.d/00-general \
    && echo 'APT::Get::force-yes "true";' >> /etc/apt/apt.conf.d/00-general


FROM debian AS racket

ARG RACKET_VERSION=8.6

RUN apt-get update -q \
    && apt-get install --no-install-recommends -q -y \
         ca-certificates curl libcurl3-gnutls \
    && rm -rf /var/lib/apt/lists/* \
    && curl -L -o racket-install.sh \
         -O http://mirror.racket-lang.org/installers/${RACKET_VERSION}/racket-${RACKET_VERSION}-x86_64-linux-cs.sh \
    && echo "yes\n1\n" | sh racket-install.sh --create-dir --unix-style --dest /usr/ \
    && rm racket-install.sh

ENV SSL_CERT_FILE="/etc/ssl/certs/ca-certificates.crt"
ENV SSL_CERT_DIR="/etc/ssl/certs"

RUN apt-get update -q \
  && apt-get install --no-install-recommends -q -y redis-server

FROM racket AS builder

RUN raco pkg install -D --auto --skip-installed redis-lib threading-lib

WORKDIR /racket
ADD  . .

RUN raco make main.rkt \
    && raco exe -o app main.rkt \
    && raco dist dist app


FROM racket

RUN apt-get update -q \
  && apt-get install --no-install-recommends -q -y gettext-base

WORKDIR /racket
COPY --from=builder /racket/dist .
ADD config config
ADD scripts scripts

EXPOSE 8080

CMD ["/racket/scripts/run"]
