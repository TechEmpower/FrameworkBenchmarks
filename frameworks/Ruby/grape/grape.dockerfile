FROM ruby:3.1

ADD ./ /grape

WORKDIR /grape

RUN bundle install --jobs=4 --gemfile=/grape/Gemfile --path=/grape/grape/bundle

EXPOSE 8080

CMD bundle exec puma -t 8:32 -w 8 --preload -b tcp://0.0.0.0:8080 -e production
