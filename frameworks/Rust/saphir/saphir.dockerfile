FROM rust:1.41

WORKDIR /saphir

ADD . .

RUN cargo clean
RUN cargo build --release

EXPOSE 8080

ENTRYPOINT [ "./target/release/saphir-techempower" ]
