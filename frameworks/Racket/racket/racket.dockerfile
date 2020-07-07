FROM debian:stretch-slim AS debian

ARG DEBIAN_FRONTEND=noninteractive
ARG TERM=linux

RUN echo 'APT::Get::Install-Recommends "false";' > /etc/apt/apt.conf.d/00-general \
    && echo 'APT::Get::Install-Suggests "false";' >> /etc/apt/apt.conf.d/00-general \
    && echo 'APT::Get::Assume-Yes "true";' >> /etc/apt/apt.conf.d/00-general \
    && echo 'APT::Get::force-yes "true";' >> /etc/apt/apt.conf.d/00-general


FROM debian AS racket

ARG RACKET_VERSION=7.7

RUN apt-get update -q \
    && apt-get install --no-install-recommends -q -y \
         ca-certificates curl libcurl3-gnutls \
    && rm -rf /var/lib/apt/lists/* \
    && curl -L -o racket-install.sh \
         -O http://mirror.racket-lang.org/installers/${RACKET_VERSION}/racket-minimal-${RACKET_VERSION}-x86_64-linux-cs.sh \
    && echo "yes\n1\n" | sh racket-install.sh --create-dir --unix-style --dest /usr/ \
    && rm racket-install.sh

ENV SSL_CERT_FILE="/etc/ssl/certs/ca-certificates.crt"
ENV SSL_CERT_DIR="/etc/ssl/certs"

RUN apt-get update -q \
  && apt-get install --no-install-recommends -q -y nginx


FROM racket AS builder

RUN raco pkg install --auto compiler-lib db-lib threading-lib web-server-lib

WORKDIR /racket
ADD  . .

RUN raco make servlet.rkt \
  && raco exe servlet.rkt


FROM racket

WORKDIR /racket
COPY --from=builder /racket/servlet .
ADD config config
ADD scripts scripts

EXPOSE 8080

CMD ["/racket/scripts/run"]
