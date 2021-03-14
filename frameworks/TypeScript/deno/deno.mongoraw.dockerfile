FROM hayd/alpine-deno:1.8.1

EXPOSE 8080

WORKDIR /app

USER deno
ENV DATABASE mongodb

COPY ./src .

RUN deno cache main.mongoraw.ts

EXPOSE 8080

CMD ["run", "--allow-net", "main.mongoraw.ts"]
