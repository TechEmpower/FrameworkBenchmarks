FROM rust:1.85 AS hyper

WORKDIR /src
ENV RUSTFLAGS="-C target-cpu=native"

# Cache dependency builds (requires passing --force-rm False to tfb command)
COPY Cargo.toml Cargo.lock /src/
RUN mkdir src \
    && echo "fn main() {println!(\"if you see this, the build broke\")}" > src/main.rs \
    && cargo build --release \
    && rm -rfv src/ target/release/hyper-techempower* target/release/deps/hyper_techempower*

COPY . /src/
RUN cargo install --path . --locked
EXPOSE 8080
CMD ["hyper-techempower"]
HEALTHCHECK CMD curl --fail http://localhost:8080/ping || exit 1