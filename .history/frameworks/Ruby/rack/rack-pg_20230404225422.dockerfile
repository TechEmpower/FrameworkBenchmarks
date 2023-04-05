FROM ruby:3.2


RUN gem update --system && \
    gem install bundler


ADD ./ /rack

WORKDIR /rack

RUN bundle install --jobs=4 --gemfile=/rack/Gemfile --path=/rack/rack/bundle

EXPOSE 8080
