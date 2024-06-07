FROM ruby:3.3

ENV RUBY_YJIT_ENABLE=1

ADD ./ /grape

WORKDIR /grape

RUN bundle install --jobs=4 --gemfile=/grape/Gemfile

EXPOSE 8080

CMD bundle exec puma -C config/puma.rb -b tcp://0.0.0.0:8080 -e production
