FROM ruby:2.4

RUN apt update -yqq && apt install -yqq nginx

ADD ./ /hanami

WORKDIR /hanami

RUN bundle install --jobs=4 --gemfile=/hanami/Gemfile --path=/hanami/hanami/bundle

CMD nginx -c /hanami/config/nginx.conf && \
    RACK_ENV=none DB_HOST=tfb-database bundle exec unicorn_rails -E production -c config/unicorn.rb
