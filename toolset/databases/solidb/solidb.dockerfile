FROM debian:trixie-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /solidb

# Install solidb via install script
RUN curl -sSL https://raw.githubusercontent.com/solisoft/solidb/main/install.sh | sh

ENV PATH="/root/.local/bin:$PATH"
ENV SOLIDB_ADMIN_PASSWORD=benchmarkdbpass
ENV SOLIDB_PORT=6745
ENV SOLIDB_DATA_DIR=/data

EXPOSE 6745

COPY entrypoint.sh /solidb/entrypoint.sh
RUN chmod +x /solidb/entrypoint.sh

CMD ["/solidb/entrypoint.sh"]
