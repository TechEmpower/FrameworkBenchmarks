FROM alpine:latest as certs
RUN apk --update add ca-certificates

FROM reasonnative/web:4.10.1-nightly as builder

RUN mkdir /app
WORKDIR /app

COPY src/esy.json src/morph-tfb.opam src/dune-project /app/

RUN esy install
RUN esy build-dependencies --release

COPY ./src/bin /app/bin

RUN esy dune build --profile=docker --release

RUN esy mv "#{self.target_dir / 'default' / 'bin' / 'tfb.exe'}" main.exe

RUN strip main.exe

FROM scratch as runtime

ENV OPENSSL_STATIC=1
ENV SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt
ENV SSL_CERT_DIR=/etc/ssl/certs
COPY --from=certs /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/

WORKDIR /app

COPY --from=builder /app/main.exe main.exe

ENTRYPOINT ["/app/main.exe"]
