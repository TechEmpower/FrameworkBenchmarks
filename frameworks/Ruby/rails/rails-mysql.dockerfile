FROM ruby:3.0

WORKDIR /rails
COPY ./Gemfile* /rails/

RUN bundle install --jobs=8 --without postgresql

COPY . /rails/

ENV RAILS_ENV=production_mysql
ENV PORT=8080
EXPOSE 8080

CMD ["rails", "server"]