FROM ruby:3.5-rc

ADD ./ /roda-sequel
WORKDIR /roda-sequel

ENV RUBY_YJIT_ENABLE=1
ENV RUBY_THREAD_TIMESLICE=10

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
RUN bundle config set with 'mysql puma'
RUN bundle install --jobs=8

ENV DBTYPE=mysql

ENV WEB_CONCURRENCY=auto
EXPOSE 8080

CMD bundle exec puma -C config/mri_puma.rb -b tcp://0.0.0.0:8080 -e production
