FROM oven/bun:1.0

EXPOSE 8080

WORKDIR /app

USER bun

COPY ./src .

ENV NODE_ENV=production

CMD ["bun", "spawn.ts"]
