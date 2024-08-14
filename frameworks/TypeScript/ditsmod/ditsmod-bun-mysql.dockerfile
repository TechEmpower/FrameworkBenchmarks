FROM oven/bun:1.1

COPY ./ ./

RUN bun install
RUN bun run build

ENV NODE_ENV production
ENV IS_BUN true
ENV DATABASE mysql
ENV MYSQL_HOST tfb-database
ENV MYSQL_USER benchmarkdbuser
ENV MYSQL_PSWD benchmarkdbpass
ENV MYSQL_DBNAME hello_world

EXPOSE 8080
CMD rm node_modules/@ditsmod/*/tsconfig.json && bun src/app/bun-integration/spawn.ts
