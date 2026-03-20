FROM oven/bun:1.3

EXPOSE 8080

WORKDIR /app

COPY ./src .

ENV NODE_ENV=production

RUN bun build --compile --minify --outfile server index.ts

USER bun

CMD ["bun", "spawn.ts"]
