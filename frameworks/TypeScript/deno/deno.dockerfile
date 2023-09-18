FROM denoland/deno:1.36.4

EXPOSE 8080

WORKDIR /app

USER deno

COPY ./src .

RUN deno cache main.ts

EXPOSE 8080

CMD ["run", "--allow-net", "--unstable", "main.ts"]
