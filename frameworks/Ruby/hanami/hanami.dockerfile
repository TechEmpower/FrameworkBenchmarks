FROM ruby:4.0

ENV RUBY_YJIT_ENABLE=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

ADD ./ /hanami
WORKDIR /hanami

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
RUN bundle config set with 'puma'
RUN bundle install --jobs=8

EXPOSE 8080

ENV HANAMI_ENV=production
ENV HANAMI_PORT=8080
ENV DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world

CMD export WEB_CONCURRENCY=$(($(nproc)*5/4)) && \
    bundle exec hanami server
