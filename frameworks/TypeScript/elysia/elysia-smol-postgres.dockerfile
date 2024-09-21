FROM oven/bun:1.1

EXPOSE 8080

COPY . .

ENV NODE_ENV production

RUN bun install

ENV DATABASE postgres

RUN bun run build

RUN sed -i 's/smol = false/smol = true/g' bunfig.toml

CMD ["bun", "./dist/index.js"]
