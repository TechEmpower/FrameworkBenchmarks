FROM ruby:3.4-rc

ENV RUBY_YJIT_ENABLE=1
ENV RUBY_MN_THREADS=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

WORKDIR /rack

COPY Gemfile  ./

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
RUN bundle config set without 'development test'
RUN bundle install --jobs=8

COPY . .

EXPOSE 8080

CMD bundle exec puma -C config/puma.rb -b tcp://0.0.0.0:8080 -e production

