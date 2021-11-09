FROM denoland/deno:latest

EXPOSE 8080

WORKDIR /app

USER deno
ENV DATABASE mongodb

COPY src/depends.ts .
RUN deno cache depends.ts

ADD . .
RUN deno cache src/main.ts

CMD ["run", "--allow-net", "src/main.mongoraw.ts"]

