FROM ruby:3.3

EXPOSE 8080
WORKDIR /rage

COPY Gemfile*  /rage/
RUN bundle install --jobs=8
COPY . /rage

ENV RUBY_YJIT_ENABLE=1
ENV RAGE_PATCH_AR_POOL=1
ENV BUNDLE_FORCE_RUBY_PLATFORM=true

CMD bundle exec rage s -b 0.0.0.0 -p 8080 -e production
