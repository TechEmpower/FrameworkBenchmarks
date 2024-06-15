FROM ruby:3.3

ENV RUBY_YJIT_ENABLE=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libjemalloc.so.2

WORKDIR /hanami

COPY Gemfile  ./

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
RUN bundle install --jobs=8

COPY . .

EXPOSE 8080

ENV HANAMI_ENV=production
ENV HANAMI_PORT=8080
ENV DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world

CMD bundle exec hanami server
