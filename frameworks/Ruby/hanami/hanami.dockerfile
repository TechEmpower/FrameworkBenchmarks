FROM ruby:3.3

ENV BUNDLE_FORCE_RUBY_PLATFORM=true
ENV RUBY_YJIT_ENABLE=1

WORKDIR /hanami

COPY Gemfile  ./

RUN bundle install --jobs=8

COPY . .

EXPOSE 8080

ENV HANAMI_ENV=production
ENV HANAMI_PORT=8080
ENV DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world

CMD bundle exec hanami server
