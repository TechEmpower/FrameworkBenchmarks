FROM ruby:2.4

WORKDIR /padrino
COPY app app
COPY config config
COPY models models
COPY .components .components
COPY config.ru config.ru
COPY Gemfile Gemfile
COPY Rakefile Rakefile

RUN bundle install --jobs=4 --gemfile=/padrino/Gemfile --path=/padrino/padrino/bundle

RUN apt-get update -yqq && apt-get install -yqq nginx

EXPOSE 8080

CMD nginx -c /padrino/config/nginx.conf && \
    bundle exec unicorn -E production -c config/unicorn.rb
