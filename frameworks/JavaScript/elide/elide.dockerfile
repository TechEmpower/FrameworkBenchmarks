FROM --platform=linux/amd64 ghcr.io/elide-dev/bench:1.0-alpha10-bench1-compat@sha256:1e679d95e18f9826c24a74d1709856849f53d3ca20c9bb25b548a8ec62424ad9 AS runtime

EXPOSE 3000

WORKDIR /app

COPY ./src .

ENV NODE_ENV=production

CMD ["elide", "serve", "server.js"]
