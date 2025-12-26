FROM ruby:4.0

ENV RUBY_YJIT_ENABLE=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

RUN apt-get update -yqq && apt-get install -yqq nginx

ADD ./ /grape

WORKDIR /grape

RUN bundle config set with 'iodine'
RUN bundle install --jobs=8 --gemfile=/grape/Gemfile

ENV RACK_ENV=production
ENV MAX_THREADS=1

EXPOSE 8080

CMD bundle exec iodine -p 8080 -w $(ruby config/auto_tune.rb | grep -Eo '[0-9]+' | head -n 1)
