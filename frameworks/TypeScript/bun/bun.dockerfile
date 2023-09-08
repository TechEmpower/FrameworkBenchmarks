FROM oven/bun:1.0

RUN mkdir /app
WORKDIR /app

COPY ./package.json /app/package.json
COPY ./bun.lockb /app/bun.lockb

RUN bun install --frozen-lockfile --production

COPY ./index.ts /app/index.ts
COPY ./db /app/db

ENV NODE_ENV production

EXPOSE 8080

CMD bun run index.ts