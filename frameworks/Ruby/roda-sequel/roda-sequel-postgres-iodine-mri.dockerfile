FROM ruby:3.4

ADD ./ /roda-sequel
WORKDIR /roda-sequel

ENV RUBY_YJIT_ENABLE=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
RUN bundle config set with 'iodine'
RUN bundle install --jobs=8

ENV DBTYPE=postgresql

EXPOSE 8080

CMD bundle exec iodine -p 8080 -w $(ruby config/auto_tune.rb | grep -Eo '[0-9]+' | head -n 1)
