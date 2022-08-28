FROM ruby:3.1

RUN apt-get update -yqq && apt-get install -yqq nginx

ADD ./ /grape

WORKDIR /grape

RUN bundle install --jobs=4 --gemfile=/grape/Gemfile --path=/grape/grape/bundle

EXPOSE 8080

CMD nginx -c /grape/config/nginx.conf && bundle exec unicorn -E production -c config/unicorn.rb
