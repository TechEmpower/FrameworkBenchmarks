FROM ruby:3.0

ENV BUNDLE_WITHOUT=postgresql
ENV RAILS_ENV=production_mysql
ENV PORT=8080

EXPOSE 8080
WORKDIR /rails

COPY ./Gemfile* /rails/

RUN bundle install --jobs=8

COPY . /rails/

CMD ["rails", "server"]