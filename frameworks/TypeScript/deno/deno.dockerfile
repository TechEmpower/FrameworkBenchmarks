FROM hayd/alpine-deno:latest

EXPOSE 8080

WORKDIR /app

USER deno
ENV DATABASE mongodb

COPY ./src .

RUN deno cache main.ts

EXPOSE 8080

RUN deno run -A main.ts
