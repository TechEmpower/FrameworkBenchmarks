FROM hayd/alpine-deno:0.41.0

EXPOSE 8080

WORKDIR /app

USER deno

COPY ./src .

RUN deno cache main.ts

CMD ["--allow-net", "main.ts"]
