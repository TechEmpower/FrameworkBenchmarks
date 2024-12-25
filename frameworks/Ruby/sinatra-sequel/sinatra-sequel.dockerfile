FROM ruby:3.4-rc

ENV RUBY_YJIT_ENABLE=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

ADD ./ /sinatra-sequel
WORKDIR /sinatra-sequel

ENV BUNDLE_WITHOUT=postgresql:passenger:unicorn
RUN bundle install --jobs=4 --gemfile=/sinatra-sequel/Gemfile

ENV DBTYPE=mysql

EXPOSE 8080

CMD bundle exec puma -C config/mri_puma.rb -b tcp://0.0.0.0:8080 -e production
