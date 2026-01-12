FROM ruby:4.0

ENV RUBY_YJIT_ENABLE=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

ADD ./ /grape

WORKDIR /grape

RUN bundle config set with 'puma'
RUN bundle install --jobs=8 --gemfile=/grape/Gemfile

ENV WEB_CONCURRENCY=auto
ENV MIN_THREADS=5
ENV MAX_THREADS=5

EXPOSE 8080

CMD bundle exec puma -b tcp://0.0.0.0:8080 -e production
