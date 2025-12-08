FROM ruby:4.0-rc

ENV RUBY_YJIT_ENABLE=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

WORKDIR /rack

COPY Gemfile* ./

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
RUN bundle config set with 'puma'
RUN bundle install --jobs=8

COPY . .

EXPOSE 8080
ENV WEB_CONCURRENCY=auto

CMD bundle exec puma -C config/puma.rb -b tcp://0.0.0.0:8080 -e production
