FROM ruby:3.5-rc

EXPOSE 8080
WORKDIR /rage-sequel

COPY Gemfile*  /rage-sequel/
RUN bundle install --jobs=8
COPY . /rage-sequel

ENV RUBY_YJIT_ENABLE=1
ENV BUNDLE_FORCE_RUBY_PLATFORM=true

CMD bundle exec rage s -b 0.0.0.0 -p 8080 -e production
