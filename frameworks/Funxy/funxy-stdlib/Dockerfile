FROM ubuntu:22.04

RUN apt-get update && \
    apt-get install -y curl ca-certificates && \
    rm -rf /var/lib/apt/lists/*

# Install Funxy binary directly (non-interactive, Docker-friendly)
RUN set -eux; \
    arch="$(dpkg --print-architecture)"; \
    case "$arch" in \
      amd64) funxy_arch="amd64" ;; \
      arm64) funxy_arch="arm64" ;; \
      *) echo "Unsupported architecture: $arch" >&2; exit 1 ;; \
    esac; \
    curl -fsSL -o /usr/local/bin/funxy "https://github.com/funvibe/funxy/releases/latest/download/funxy-linux-${funxy_arch}"; \
    chmod +x /usr/local/bin/funxy

WORKDIR /app
COPY supervisor.lang worker.lang ./

EXPOSE 8080

CMD ["funxy", "vmm", "supervisor.lang"]