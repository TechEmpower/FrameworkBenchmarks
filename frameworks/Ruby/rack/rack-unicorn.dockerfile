FROM ruby:3.3

ENV RUBY_YJIT_ENABLE=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libjemalloc.so.2

WORKDIR /rack

COPY Gemfile ./

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
RUN bundle config set without 'development test'
RUN bundle install --jobs=8

COPY . .

EXPOSE 8080

#CMD nginx -c /rack/config/nginx.conf && bundle exec unicorn -E production -c config/unicorn.rb

CMD bundle exec unicorn -c config/unicorn.rb -o 0.0.0.0 -p 8080 -E production
