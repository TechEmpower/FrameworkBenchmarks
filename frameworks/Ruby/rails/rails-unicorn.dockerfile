FROM ruby:3.0

RUN apt-get update -yqq && apt-get install -yqq nginx

ADD ./ /rails

WORKDIR /rails

RUN bundle install --jobs=4 --gemfile=/rails/Gemfile --path=/rails/rails/bundle --without=postgresql

EXPOSE 8080

CMD nginx -c /rails/config/nginx.conf && \
  DB_HOST=tfb-database bundle exec unicorn_rails -E production_mysql -c config/unicorn.rb
