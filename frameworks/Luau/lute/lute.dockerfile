FROM ubuntu:24.04

EXPOSE 3000

WORKDIR /app

RUN apt-get update && apt-get install -y curl unzip

COPY rokit.toml .

RUN curl -sSf https://raw.githubusercontent.com/rojo-rbx/rokit/main/scripts/install.sh | bash

ENV PATH="/root/.rokit/bin:${PATH}"

RUN rokit install --no-trust-check

COPY ./src .

CMD ["sh", "-c", "lute parallel.luau -- $(nproc)"]
