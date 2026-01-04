FROM ruby:3.5-rc

ENV RUBY_YJIT_ENABLE=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

RUN apt-get install -y build-essential libclang-dev

WORKDIR /rack

COPY Gemfile* ./

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
RUN bundle config set with 'itsi'
RUN bundle install --jobs=8

COPY . .

ENV MAX_THREADS=5

EXPOSE 8080

CMD bundle exec itsi start -C config/itsi.rb --bind "http://tfb-server:8080"
