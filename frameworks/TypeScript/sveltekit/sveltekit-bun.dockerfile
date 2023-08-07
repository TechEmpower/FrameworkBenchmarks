###################
# BUILD
###################

FROM node:hydrogen-slim as builder

WORKDIR /app

COPY . .

RUN npm install -g bun

RUN bun install

RUN bun run check

ENV RUNTIME bun

RUN bun run build

###################
# EXTRACTION
###################

FROM oven/bun:latest

WORKDIR /app

ENV NODE_ENV production
ENV RUNTIME bun

ENV PORT 8080
EXPOSE 8080

COPY --from=builder /app/package.json .
COPY --from=builder /app/bun.lockb .

RUN bun install --production

COPY --from=builder /app/build ./build
COPY --from=builder /app/spawn.ts .

CMD ["bun", "spawn.ts"]
