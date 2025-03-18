FROM oven/bun:1.1

COPY ./ ./

RUN bun install
RUN bun run build

ENV NODE_ENV production

EXPOSE 8080
CMD rm node_modules/@ditsmod/*/tsconfig.json && bun dist/main.js
