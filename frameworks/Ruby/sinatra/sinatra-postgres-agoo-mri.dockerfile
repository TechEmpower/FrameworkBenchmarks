FROM ruby:3.4

ENV RUBY_YJIT_ENABLE=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

ADD ./ /sinatra
WORKDIR /sinatra

ENV BUNDLE_WITH=postgresql:agoo
RUN bundle install --jobs=4 --gemfile=/sinatra/Gemfile

ENV DBTYPE=postgresql

EXPOSE 8080

CMD RACK_ENV=production bundle exec rackup -r agoo -s agoo -p 8080 -q -O workers=$(ruby config/auto_tune.rb | grep -Eo '[0-9]+' | head -n 1)
