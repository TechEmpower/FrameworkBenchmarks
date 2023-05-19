ARG ELIXIR="1.14.4"
ARG ERLANG="26.0"
ARG ALPINE="3.17.3"

ARG BUILDER_IMAGE="hexpm/elixir:${ELIXIR}-erlang-${ERLANG}-alpine-${ALPINE}"
ARG RUNNER_IMAGE="alpine:${ALPINE}"

FROM ${BUILDER_IMAGE} AS builder

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

# start a new build stage so that the final image will only contain
# the compiled release and other runtime necessities
FROM ${RUNNER_IMAGE}

RUN apk add --no-cache libstdc++ openssl ncurses-libs

COPY --from=builder /export /opt

EXPOSE 8080

ENTRYPOINT ["/opt/bin/hello"]
CMD ["start"]
