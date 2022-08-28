FROM erlang:25 AS builder

RUN apt-get update -y && \
  apt-get install -y libicu-dev

# elixir expects utf8.
ENV ELIXIR_VERSION="v1.13.4" \
  LANG=C.UTF-8

RUN set -xe \
  && ELIXIR_DOWNLOAD_URL="https://github.com/elixir-lang/elixir/archive/${ELIXIR_VERSION}.tar.gz" \
  && ELIXIR_DOWNLOAD_SHA256="95daf2dd3052e6ca7d4d849457eaaba09de52d65ca38d6933c65bc1cdf6b8579" \
  && curl -fSL -o elixir-src.tar.gz $ELIXIR_DOWNLOAD_URL \
  && echo "$ELIXIR_DOWNLOAD_SHA256  elixir-src.tar.gz" | sha256sum -c - \
  && mkdir -p /usr/local/src/elixir \
  && tar -xzC /usr/local/src/elixir --strip-components=1 -f elixir-src.tar.gz \
  && rm elixir-src.tar.gz \
  && cd /usr/local/src/elixir \
  && make install clean \
  && find /usr/local/src/elixir/ -type f -not -regex "/usr/local/src/elixir/lib/[^\/]*/lib.*" -exec rm -rf {} + \
  && find /usr/local/src/elixir/ -type d -depth -empty -delete

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

FROM debian:bullseye-slim AS app

RUN apt-get update -y && \
  apt-get install -y openssl libicu-dev

ENV LANG=C.UTF-8

EXPOSE 8080

RUN adduser -h /home/app -D app
WORKDIR /home/app
COPY --from=builder /app/_build .

# Run the Phoenix app
CMD ["./prod/rel/framework_benchmarks/bin/framework_benchmarks", "start"]
