FROM ruby:3.4

RUN apt-get update -yqq && apt-get install -yqq --no-install-recommends redis-server

EXPOSE 8080
WORKDIR /rails

# ENV RUBY_YJIT_ENABLE=1 YJIT is enabled in config/initializers/enable_yjit.rb

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

COPY ./Gemfile* /rails/

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
ENV BUNDLE_WITH=postgresql:pitchfork
RUN bundle install --jobs=8

COPY . /rails/

ENV RAILS_ENV=production_postgresql
ENV PORT=8080
ENV REDIS_URL=redis://localhost:6379/0
CMD service redis-server start && \
    RACK_ENV=production bundle exec pitchfork -c config/pitchfork.rb
