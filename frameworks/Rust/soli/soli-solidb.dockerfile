FROM debian:trixie-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Install soli via install script
RUN curl -sSL https://raw.githubusercontent.com/solisoft/soli_lang/main/install.sh | sh
  
ENV PATH="/root/.local/bin:$PATH"

# Copy benchmark application files
COPY app/ /app/app/
COPY config/ /app/config/

# Copy entrypoint script
COPY entrypoint.sh /app/entrypoint.sh
RUN chmod +x /app/entrypoint.sh

EXPOSE 3000

CMD ["/app/entrypoint.sh"]
