FROM denoland/deno:2.3.1

EXPOSE 8080

WORKDIR /app

USER deno

ENV PG_NAME hello_world
ENV PG_HOST tfb-database
ENV PG_USER benchmarkdbuser
ENV PG_PSWD benchmarkdbpass
ENV DATABASE postgres

COPY . .

RUN deno install --entrypoint ./src/main.ts

CMD ["deno", "serve", "--parallel", "--port", "8080", "--host", "0.0.0.0", "-A", "./src/main.ts"]
