FROM rust:latest
ENV DEBIAN_FRONTEND=noninteractive
WORKDIR /app
RUN rustup install nightly
RUN git clone https://github.com/TachyonConcepts/TachyonConcept .
RUN RUSTFLAGS="-C target-cpu=native -C opt-level=3 -C target-feature=+avx2,+bmi2" cargo install --path .
RUN cargo clean
RUN chmod +x /app/run_ub.sh
EXPOSE 8080
CMD /app/run_ub.sh