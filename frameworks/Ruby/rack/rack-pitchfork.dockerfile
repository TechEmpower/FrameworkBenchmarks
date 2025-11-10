FROM ruby:3.5-rc

ENV RUBY_YJIT_ENABLE=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

RUN apt-get install -yqq nginx

WORKDIR /rack

COPY Gemfile* ./

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
RUN bundle config set with 'pitchfork'
RUN bundle install --jobs=8

COPY . .

EXPOSE 8080

CMD nginx -c /rack/config/nginx.conf && \
    bundle exec pitchfork -c config/pitchfork.rb -E production
