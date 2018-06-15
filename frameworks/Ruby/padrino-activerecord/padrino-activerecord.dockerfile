FROM ruby:2.4

WORKDIR /padrino-activerecord
COPY app app
COPY config config
COPY exe exe
COPY lib lib
COPY models models
COPY .components .components
COPY config.ru config.ru
COPY Gemfile Gemfile
COPY Rakefile Rakefile

RUN bundle install --jobs=4 --gemfile=/padrino-activerecord/Gemfile --path=/padrino-activerecord/padrino/bundle

CMD bundle exec puma -C config/puma.rb -b tcp://0.0.0.0:8080 --preload -e production
