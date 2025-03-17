FROM ruby:3.4

WORKDIR /padrino
COPY app app
COPY config config
COPY models models
COPY .components .components
COPY config.ru config.ru
COPY Gemfile Gemfile
COPY Rakefile Rakefile

RUN bundle config set with 'puma'
RUN bundle install --jobs=4 --gemfile=/padrino/Gemfile

EXPOSE 8080

ENV RUBY_YJIT_ENABLE=1

CMD bundle exec puma -C config/puma.rb -w 8 --preload
