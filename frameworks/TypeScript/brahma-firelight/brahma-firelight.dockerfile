FROM oven/bun:1.3 AS builder
WORKDIR /app

COPY package.json bun.lock ./

RUN bun ci

COPY . .
RUN bun run build

FROM oven/bun:1.3 AS runtime
WORKDIR /app

ENV NODE_ENV=production
ENV PORT=8080

COPY --from=builder /app/dist ./dist
COPY --from=builder /app/node_modules ./node_modules
COPY --from=builder /app/package.json ./package.json

ENV NODE_ENV=production

EXPOSE 8080

ENTRYPOINT ["bun", "dist/main.js"]
