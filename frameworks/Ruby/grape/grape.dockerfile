FROM tfb/ruby-2.4:latest

ADD ./ /grape

WORKDIR /grape

RUN bundle install --jobs=4 --gemfile=/grape/Gemfile --path=/grape/grape/bundle

CMD bundle exec puma -t 8:32 -w 8 --preload -b tcp://0.0.0.0:8080 -e production
