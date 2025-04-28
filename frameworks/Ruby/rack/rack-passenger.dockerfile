FROM ruby:3.5-rc

ENV RUBY_YJIT_ENABLE=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

WORKDIR /rack

COPY Gemfile ./

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
RUN bundle config set with 'passenger'
RUN bundle install --jobs=8

COPY . .

# TODO: https://github.com/phusion/passenger/issues/1916
ENV _PASSENGER_FORCE_HTTP_SESSION=true

RUN ruby config/auto_tune.rb | grep -Eo '[0-9]+' | head -n 1 > instances

EXPOSE 8080

CMD bundle exec passenger start --log-level 1 \
       --engine builtin --disable-turbocaching --disable-security-update-check \
       --spawn-method direct --max-pool-size $(cat instances) --min-instances $(cat instances) --max-request-queue-size 1024 \
       --address 0.0.0.0 --port 8080 --environment production

