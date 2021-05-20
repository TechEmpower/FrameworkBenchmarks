FROM ruby:3.0

RUN apt-get update -yqq && apt-get install -yqq --no-install-recommends redis-server

EXPOSE 8080
WORKDIR /rails

COPY ./Gemfile* /rails/

ENV BUNDLE_WITHOUT=postgresql
RUN bundle install --jobs=8

COPY . /rails/

ENV RAILS_ENV=production_mysql
ENV PORT=8080
ENV REDIS_URL=redis://localhost:6379/0/cache
CMD ./run-with-redis.sh