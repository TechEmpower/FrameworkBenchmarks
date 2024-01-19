FROM ruby:3.3

RUN apt-get update -q \
    && apt-get install --no-install-recommends -q -y \
         build-essential \
         libpq-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /rack

COPY Gemfile app.rb ./

RUN bundle install --jobs=4

EXPOSE 8080

ENV RUBY_YJIT_ENABLE=1

CMD AGOO_WORKER_COUNT=$(nproc) ruby app.rb
