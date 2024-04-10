FROM ruby:3.3

ENV RUBY_YJIT_ENABLE=1

RUN apt-get update -yqq && apt-get install -yqq nginx

ADD ./ /grape

WORKDIR /grape

RUN bundle install --jobs=4 --gemfile=/grape/Gemfile

EXPOSE 8080

CMD nginx -c /grape/config/nginx.conf && bundle exec unicorn -E production -c config/unicorn.rb
