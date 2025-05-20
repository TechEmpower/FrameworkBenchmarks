FROM ruby:3.5-rc

ENV RUBY_YJIT_ENABLE=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

WORKDIR /rack

COPY Gemfile ./

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
RUN bundle config set with 'unicorn'
RUN bundle install --jobs=8

COPY . .

EXPOSE 8080

CMD bundle exec unicorn -c config/unicorn.rb -o 0.0.0.0 -p 8080 -E production
