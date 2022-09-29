###############
# Build stage #
###############
FROM elixir:1.14.0 as build

ARG MIX_ENV="prod"

RUN mix local.hex --force && \
    mix local.rebar --force

COPY mix.exs mix.lock ./
RUN mix deps.get --force --only prod

COPY config ./config

RUN mix deps.compile

COPY priv ./priv
COPY lib ./lib

COPY rel ./rel
RUN mix release --force --path /export

####################
# Deployment Stage #
####################
FROM erlang:25.1

COPY --from=build /export /opt

EXPOSE 8080

ENTRYPOINT ["/opt/bin/hello"]
CMD ["start"]
