FROM rust:1.68.2

ENV TECHEMPOWER_MONGODB_URL=mongodb://tfb-database:27017
ENV TECHEMPOWER_MAX_POOL_SIZE=28
ENV TECHEMPOWER_MIN_POOL_SIZE=14

ADD ./ /salvo
WORKDIR /salvo

ENV RUSTFLAGS "-C target-cpu=native"
RUN cargo build --release

EXPOSE 8080

CMD ./target/release/main-mongo
