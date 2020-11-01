FROM reasonnative/web:4.10.1-nightly as builder

RUN mkdir /app
WORKDIR /app

COPY src/esy.json src/morph-tfb.opam src/dune-project /app/

RUN esy install
RUN esy build-dependencies --release

COPY ./src/bin /app/bin

RUN esy build --release

ENTRYPOINT ["esy", "x", "--release", "tfb"]
