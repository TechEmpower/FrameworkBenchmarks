FROM maxmcd/deno

WORKDIR /home
COPY src src

CMD ["deno", "--allow-net", "./src/index.ts"]
