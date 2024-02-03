FROM ruby:3.3

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
ENV RUBY_YJIT_ENABLE=1

WORKDIR /rack

COPY Gemfile  ./

RUN bundle config set without 'development test'
RUN bundle install --jobs=8

COPY . .

EXPOSE 8080

CMD bundle exec puma -C config/puma.rb -b tcp://0.0.0.0:8080 -e production

