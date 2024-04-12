FROM ruby:3.0

WORKDIR /padrino
COPY app app
COPY config config
COPY models models
COPY .components .components
COPY config.ru config.ru
COPY Gemfile Gemfile
COPY Rakefile Rakefile

RUN bundle install --jobs=4 --gemfile=/padrino/Gemfile

EXPOSE 8080

CMD ["bundle", "exec", "puma", "-C", "config/puma.rb", "-w", "8", "--preload"]
