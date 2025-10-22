FROM rust:1.70
ENV RUSTFLAGS "-C target-cpu=native"
# Needed for using snmalloc_rs
RUN apt-get update -yqq && apt-get install -yqq cmake g++
WORKDIR /app

COPY . .
RUN cargo build --release --bin api
EXPOSE 8000
CMD ./target/release/api