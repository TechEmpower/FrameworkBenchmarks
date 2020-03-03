FROM elixir:1.9.4

WORKDIR /phoenix

COPY config ./config
COPY lib ./lib
COPY priv ./priv
COPY web ./web
COPY mix.exs .
COPY mix.lock .

ENV MIX_ENV=prod

RUN mix local.hex --force
RUN mix local.rebar --force
RUN mix deps.get --force --only prod
RUN mix compile --force

CMD ["elixir", "--erl", "+K true +sbwt very_long +swt very_low", "-S", "mix", "phx.server"]
