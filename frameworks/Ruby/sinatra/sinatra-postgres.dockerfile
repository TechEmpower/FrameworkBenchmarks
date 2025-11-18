FROM ruby:3.5-rc

ENV RUBY_YJIT_ENABLE=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

ADD ./ /sinatra
WORKDIR /sinatra

ENV BUNDLE_WITH=postgresql:puma
RUN bundle install --jobs=4 --gemfile=/sinatra/Gemfile

ENV WEB_CONCURRENCY=auto
ENV DBTYPE=postgresql

EXPOSE 8080

CMD bundle exec puma -C config/mri_puma.rb -b tcp://0.0.0.0:8080 -e production
