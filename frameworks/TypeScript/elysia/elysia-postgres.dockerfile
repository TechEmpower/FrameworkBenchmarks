FROM oven/bun:1.0

EXPOSE 8080

COPY . .

ENV NODE_ENV production

RUN bun install --production

RUN bun run build

ENV DATABASE postgres

CMD ["bun", "spawn.ts"]
