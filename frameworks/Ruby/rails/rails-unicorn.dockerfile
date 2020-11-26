FROM ruby:2.6

RUN apt-get update -yqq && apt-get install -yqq nginx

ADD ./ /rails

WORKDIR /rails

RUN bundle install --jobs=4 --gemfile=/rails/Gemfile --path=/rails/rails/bundle --without=postgresql

CMD nginx -c /rails/config/nginx.conf && \
  DB_HOST=tfb-database bundle exec unicorn_rails -E production_mysql -c config/unicorn.rb
