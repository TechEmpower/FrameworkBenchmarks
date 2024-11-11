FROM denoland/deno:1.46.1

EXPOSE 8080

WORKDIR /app

USER deno

COPY ./src .

RUN deno cache main.ts

EXPOSE 8080

CMD ["deno", "serve", "--parallel", "--port", "8080", "--host", "0.0.0.0", "-A", "main.ts"]
