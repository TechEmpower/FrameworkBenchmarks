FROM ruby:4.0-rc

ENV RUBY_YJIT_ENABLE=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

ADD ./ /sinatra
WORKDIR /sinatra

ENV BUNDLE_WITH=mysql:puma
RUN bundle install --jobs=4 --gemfile=/sinatra/Gemfile

ENV APP_ENV=production
ENV DBTYPE=mysql

ENV WEB_CONCURRENCY=auto
EXPOSE 8080

CMD bundle exec puma -C config/puma.rb -b tcp://0.0.0.0:8080
