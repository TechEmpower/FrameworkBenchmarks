FROM elixir:1.11.4

WORKDIR /phoenix

RUN mix local.hex --force && \
    mix local.rebar --force

COPY config ./config
COPY lib ./lib
COPY priv ./priv
COPY web ./web
COPY mix.exs .
COPY mix.lock .

ENV MIX_ENV=prod

RUN mix do deps.get --force --only prod
RUN mix release --force

EXPOSE 8080

CMD ["_build/prod/rel/hello/bin/hello", "start"]
