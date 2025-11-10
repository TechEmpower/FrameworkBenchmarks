FROM rust:1.85 AS hyper

WORKDIR /src
COPY . .
RUN RUSTFLAGS="-C target-cpu=native" cargo install --path . --locked
EXPOSE 8080
CMD ["hyper-techempower"]
HEALTHCHECK CMD curl --fail http://localhost:8080/ping || exit 1