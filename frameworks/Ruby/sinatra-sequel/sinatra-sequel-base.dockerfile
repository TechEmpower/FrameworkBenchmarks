FROM ruby:3.4-rc

ENV RUBY_YJIT_ENABLE=1

ADD ./ /sinatra-sequel
WORKDIR /sinatra-sequel

RUN bundle install --jobs=4 --gemfile=/sinatra-sequel/Gemfile
