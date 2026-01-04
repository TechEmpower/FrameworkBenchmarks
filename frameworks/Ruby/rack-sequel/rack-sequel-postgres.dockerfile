FROM ruby:4.0

ADD ./ /rack-sequel

WORKDIR /rack-sequel

ENV RUBY_YJIT_ENABLE=1
ENV RUBY_MN_THREADS=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

RUN bundle config set with 'postgresql puma'
RUN bundle install --jobs=4 --gemfile=/rack-sequel/Gemfile

ENV DBTYPE=postgresql

ENV MAX_THREADS=8

EXPOSE 8080

CMD export WEB_CONCURRENCY=auto && \
    bundle exec puma -C config/puma.rb -b tcp://0.0.0.0:8080 -e production
