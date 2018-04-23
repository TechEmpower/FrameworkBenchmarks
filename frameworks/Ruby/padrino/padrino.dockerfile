FROM ruby:2.4

ADD ./ /padrino

WORKDIR /padrino

RUN bundle install --jobs=4 --gemfile=/padrino/Gemfile --path=/padrino/padrino/bundle

CMD DB_HOST=tfb-database bundle exec puma -C config/puma.rb -w 8 --preload
