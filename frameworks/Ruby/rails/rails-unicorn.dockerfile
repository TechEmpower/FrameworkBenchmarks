FROM ruby:2.4

RUN apt update -yqq && apt install -yqq nginx

ADD ./ /rails

WORKDIR /rails

RUN bundle install --jobs=4 --gemfile=/rails/Gemfile --path=/rails/rails/bundle

CMD nginx -c /rails/config/nginx.conf && \
    DB_HOST=tfb-database bundle exec unicorn_rails -E production -c config/unicorn.rb
