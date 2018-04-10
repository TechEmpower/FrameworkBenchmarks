FROM ruby:2.4

RUN apt update -yqq && apt install -yqq nginx

ADD ./ /grape

WORKDIR /grape

RUN bundle install --jobs=4 --gemfile=/grape/Gemfile --path=/grape/grape/bundle

CMD nginx -c /grape/config/nginx.conf && bundle exec unicorn -E production -c config/unicorn.rb
