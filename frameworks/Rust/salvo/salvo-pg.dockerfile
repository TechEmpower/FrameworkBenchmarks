FROM rust:latest

ADD ./ /salvo
WORKDIR /salvo

RUN cargo build --release

EXPOSE 8080

CMD ./target/release/main-pg
