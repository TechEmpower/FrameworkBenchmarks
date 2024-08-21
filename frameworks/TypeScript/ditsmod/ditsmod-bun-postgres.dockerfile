FROM oven/bun:1.1

COPY ./ ./

RUN bun install
RUN bun run build

ENV NODE_ENV production
ENV DATABASE postgres
ENV PG_HOST tfb-database
ENV PG_USER benchmarkdbuser
ENV PG_PSWD benchmarkdbpass
ENV PG_DBNAME hello_world

EXPOSE 8080
CMD rm node_modules/@ditsmod/*/tsconfig.json && bun dist/main.js
