FROM ruby:4.0-rc

WORKDIR /padrino
COPY app app
COPY config config
COPY models models
COPY .components .components
COPY config.ru config.ru
COPY Gemfile Gemfile
COPY Gemfile.lock Gemfile.lock
COPY Rakefile Rakefile

RUN bundle config set with 'iodine'
RUN bundle install --jobs=4 --gemfile=/padrino/Gemfile

EXPOSE 8080

ENV RUBY_YJIT_ENABLE=1
ENV RACK_ENV=production

CMD bundle exec iodine -p 8080 -w $(ruby config/auto_tune.rb | grep -Eo '[0-9]+' | head -n 1)
