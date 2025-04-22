FROM ruby:3.4

ENV RUBY_YJIT_ENABLE=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

WORKDIR /rack

COPY Gemfile ./

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
RUN bundle config set with 'pitchfork'
RUN bundle install --jobs=8

COPY . .

EXPOSE 8080

CMD bundle exec pitchfork -c config/pitchfork.rb -o 0.0.0.0 -p 8080 -E production
