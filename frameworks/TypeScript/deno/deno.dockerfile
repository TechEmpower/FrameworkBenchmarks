FROM hayd/alpine-deno:1.0.0

EXPOSE 8080

WORKDIR /app

USER deno

COPY ./src .

RUN deno cache main.ts

EXPOSE 8080

CMD ["run", "--allow-net", "main.ts"]
