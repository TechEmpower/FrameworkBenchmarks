FROM ruby:4.0

ENV RUBY_YJIT_ENABLE=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

ADD ./ /sinatra
WORKDIR /sinatra

ENV BUNDLE_WITH=postgresql:puma
RUN bundle install --jobs=4 --gemfile=/sinatra/Gemfile

ENV APP_ENV=production
ENV DBTYPE=postgresql

ENV MIN_THREADS=5
ENV MAX_THREADS=5

EXPOSE 8080

CMD bundle exec puma -b tcp://0.0.0.0:8080
