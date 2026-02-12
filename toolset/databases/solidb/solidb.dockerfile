FROM rust:1.93.0 AS builder

RUN apt-get update && apt-get install -y --no-install-recommends \
    clang \
    libclang-dev \
    && rm -rf /var/lib/apt/lists/*

RUN cargo install solidb --locked

FROM debian:testing-20260202-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /solidb

COPY --from=builder /usr/local/cargo/bin/solidb /usr/local/bin/solidb

ENV SOLIDB_ADMIN_PASSWORD=benchmarkdbpass
ENV SOLIDB_PORT=6745
ENV SOLIDB_DATA_DIR=/data

EXPOSE 6745

COPY entrypoint.sh /solidb/entrypoint.sh
RUN chmod +x /solidb/entrypoint.sh

CMD ["/solidb/entrypoint.sh"]
