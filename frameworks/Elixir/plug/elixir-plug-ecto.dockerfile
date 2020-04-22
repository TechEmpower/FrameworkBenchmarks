FROM elixir:1.9.4 as builder

RUN apt-get update -y && \
  apt-get install -y libicu-dev

ENV MIX_ENV=prod \
  LANG=C.UTF-8

RUN mkdir /app
WORKDIR /app

COPY config ./config
COPY lib ./lib
COPY mix.exs .
COPY mix.lock .

RUN mix local.hex --force
RUN mix local.rebar --force
RUN mix deps.get
RUN mix deps.compile
RUN mix release

FROM debian:buster-slim AS app

RUN apt-get update -y && \
  apt-get install -y openssl libicu-dev

ENV LANG=C.UTF-8

EXPOSE 8080

RUN adduser -h /home/app -D app
WORKDIR /home/app
COPY --from=builder /app/_build .

# Run the Phoenix app
CMD ["./prod/rel/framework_benchmarks/bin/framework_benchmarks", "start"]
