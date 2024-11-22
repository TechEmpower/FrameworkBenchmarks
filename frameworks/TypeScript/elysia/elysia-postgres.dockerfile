FROM oven/bun:1.1

EXPOSE 8080

COPY . .

ENV NODE_ENV production

RUN bun install --production

ENV DATABASE postgres

RUN bun run build

CMD ["bun", "./dist/index.js"]
