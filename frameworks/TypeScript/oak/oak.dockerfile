FROM denoland/deno:2.3.1

EXPOSE 8080

WORKDIR /app

USER deno

COPY . .

RUN deno install --entrypoint ./src/main.ts

CMD ["deno", "serve", "--parallel", "--port", "8080", "--host", "0.0.0.0", "-A", "./src/main.ts"]
