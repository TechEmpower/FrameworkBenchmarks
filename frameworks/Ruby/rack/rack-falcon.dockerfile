FROM ruby:2.4

RUN apt-get update -yqq && apt-get install -yqq nginx

ADD ./ /rack

WORKDIR /rack

RUN bundle install --jobs=4 --gemfile=/rack/Gemfile --path=/rack/rack/bundle

EXPOSE 8080

CMD bundle exec falcon serve --forked --bind tcp://0.0.0.0:8080
