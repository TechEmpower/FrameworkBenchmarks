FROM oven/bun:1.3

EXPOSE 8080

WORKDIR /app

COPY ./src .

ENV NODE_ENV=production

# PostgreSQL variant: spawn.ts spawns multiple bun processes with reusePort.
# No --compile step: bun:sql requires the full Bun runtime for DB connectivity.
USER bun

CMD ["bun", "spawn.ts"]
