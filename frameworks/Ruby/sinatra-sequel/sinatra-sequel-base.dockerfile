FROM ruby:2.4

ADD ./ /sinatra-sequel
WORKDIR /sinatra-sequel

RUN bundle install --jobs=4 --gemfile=/sinatra-sequel/Gemfile --path=/sinatra-sequel/sinatra-sequel/bundle
