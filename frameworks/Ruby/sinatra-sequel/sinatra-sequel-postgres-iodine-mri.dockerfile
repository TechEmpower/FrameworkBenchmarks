FROM ruby:3.4

ENV RUBY_YJIT_ENABLE=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

ADD ./ /sinatra-sequel
WORKDIR /sinatra-sequel

ENV BUNDLE_WITH=postgresql:iodine
RUN bundle install --jobs=4 --gemfile=/sinatra-sequel/Gemfile

ENV DBTYPE=postgresql

EXPOSE 8080

CMD bundle exec iodine -p 8080 -w $(ruby config/auto_tune.rb | grep -Eo '[0-9]+' | head -n 1)
