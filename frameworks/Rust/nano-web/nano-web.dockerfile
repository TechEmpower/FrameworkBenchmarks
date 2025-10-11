FROM docker.io/rust:1.88-slim-bookworm AS builder

WORKDIR /build
COPY ./Cargo.toml ./Cargo.lock ./VERSION /build/
RUN cargo fetch
COPY ./src/ /build/src
ENV RUSTFLAGS="-C target-cpu=native"
RUN cargo build --release --bin nano-web

FROM gcr.io/distroless/cc-debian12
COPY --from=builder /build/target/release/nano-web /app/nano-web
COPY ./techempower/public /public
EXPOSE 8000
CMD ["/app/nano-web", "serve", "/public", "--port", "8000"]
