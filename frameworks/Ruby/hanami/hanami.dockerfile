FROM ruby:3.5-rc

ENV RUBY_YJIT_ENABLE=1
ENV WEB_CONCURRENCY=auto

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

ADD ./ /hanami
WORKDIR /hanami

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
RUN bundle install --jobs=8

EXPOSE 8080

ENV HANAMI_ENV=production
ENV HANAMI_PORT=8080
ENV DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world

CMD bundle exec hanami server
