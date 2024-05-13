FROM ruby:3.3

RUN apt-get update -q \
    && apt-get install --no-install-recommends -q -y \
         build-essential \
         libpq-dev \
    && rm -rf /var/lib/apt/lists/*

ENV RUBY_YJIT_ENABLE=1

# Use Jemalloc
RUN apt-get update && \
    apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libjemalloc.so.2

WORKDIR /rack

COPY Gemfile app.rb ./

RUN bundle install --jobs=4

EXPOSE 8080


CMD AGOO_WORKER_COUNT=$(nproc) ruby app.rb
