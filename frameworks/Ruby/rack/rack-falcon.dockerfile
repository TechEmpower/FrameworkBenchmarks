FROM ruby:3.2

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
ENV RUBY_YJIT_ENABLE=1

WORKDIR /rack

COPY Gemfile Gemfile.lock ./

RUN bundle config set without 'development test'
RUN bundle install --jobs=8

COPY . .

EXPOSE 8080

CMD bundle exec falcon host
