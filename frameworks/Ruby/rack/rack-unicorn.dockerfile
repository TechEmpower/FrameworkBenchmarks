FROM ruby:2.4

RUN apt update -yqq && apt install -yqq nginx

ADD ./ /rack

WORKDIR /rack

RUN bundle install --jobs=4 --gemfile=/rack/Gemfile --path=/rack/rack/bundle

CMD nginx -c /rack/config/nginx.conf && bundle exec unicorn -E production -c config/unicorn.rb
