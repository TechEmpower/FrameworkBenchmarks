FROM elixir:1.5.3

ADD ./ /cowboy
WORKDIR /cowboy

ENV MIX_ENV=prod
RUN mix local.hex --force
RUN mix local.rebar --force
RUN mix deps.get --force --only prod
RUN mix compile --force

CMD ["elixir", "--erl", "+K true +sbwt very_long +swt very_low", "--no-halt", "-S", "mix"]
