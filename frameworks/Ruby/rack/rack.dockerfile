FROM ruby:2.4

ADD ./ /rack

WORKDIR /rack

RUN bundle install --jobs=4 --gemfile=/rack/Gemfile --path=/rack/rack/bundle

CMD bundle exec puma -t 8:32 -w 8 --preload -b tcp://0.0.0.0:8080 -e production
