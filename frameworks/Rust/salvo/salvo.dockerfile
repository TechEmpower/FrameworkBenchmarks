FROM rust:1.74

ADD ./ /salvo
WORKDIR /salvo

ENV RUSTFLAGS "-C target-cpu=native"
RUN cargo build --release

EXPOSE 8080

CMD ./target/release/main
