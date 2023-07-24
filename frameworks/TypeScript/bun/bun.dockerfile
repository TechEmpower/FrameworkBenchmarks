# syntax=docker/dockerfile:1
FROM  oven/bun:0.7

WORKDIR /app

ENV NODE_ENV production

COPY . .

RUN bun install

EXPOSE 8080

CMD ["bun", "run", "index.ts"]