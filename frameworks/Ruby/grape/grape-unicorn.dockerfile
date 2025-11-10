FROM ruby:3.5-rc

ENV RUBY_YJIT_ENABLE=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

RUN apt-get update -yqq && apt-get install -yqq nginx

ADD ./ /grape

WORKDIR /grape

RUN bundle install --jobs=4 --gemfile=/grape/Gemfile

EXPOSE 8080

CMD nginx -c /grape/config/nginx.conf && bundle exec unicorn -E production -c config/unicorn.rb
