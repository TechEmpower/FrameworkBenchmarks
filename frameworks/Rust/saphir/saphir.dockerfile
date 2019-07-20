FROM rust:1.36

WORKDIR /saphir

ADD . .

RUN cargo clean
RUN cargo build --release

EXPOSE 8080

ENTRYPOINT [ "./target/release/saphir-techempower" ]
