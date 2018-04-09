FROM ruby:2.4

RUN apt update -yqq && apt install -yqq nginx

ADD ./ /padrino

WORKDIR /padrino

RUN bundle install --jobs=4 --gemfile=/padrino/Gemfile --path=/padrino/padrino/bundle

CMD nginx -c /padrino/config/nginx.conf && \
    DB_HOST=tfb-database bundle exec unicorn -E production -c config/unicorn.rb
