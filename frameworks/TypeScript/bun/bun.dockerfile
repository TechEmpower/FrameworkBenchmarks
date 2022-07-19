FROM debian:bookworm as build

RUN apt-get update && apt-get upgrade -y && apt-get install -y curl unzip

RUN curl -fsSL https://bun.sh/install | bash

ENV BUN_INSTALL="/root/.bun"
ENV PATH="/root/.bun/bin:$PATH"

RUN mkdir /app
WORKDIR /app

COPY ./package.json /app/package.json
COPY ./bun.lockb /app/bun.lockb

RUN bun install --frozen-lockfile --production

FROM debian:bookworm-slim

RUN apt-get update && apt-get upgrade -y

RUN mkdir /app
WORKDIR /app

COPY --from=build /root/.bun /root/.bun
COPY --from=build /app/package.json ./package.json
COPY --from=build /app/bun.lockb ./bun.lockb
COPY --from=build /app/node_modules ./node_modules
COPY ./index.ts /app/index.ts
COPY ./db /app/db

ENV NODE_ENV production
ENV BUN_INSTALL="/root/.bun"
ENV PATH="/root/.bun/bin:$PATH"

EXPOSE 8080

CMD bun run index.ts