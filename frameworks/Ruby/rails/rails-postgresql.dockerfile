FROM ruby:3.0

ADD ./ /rails

WORKDIR /rails

RUN bundle install --jobs=4 --gemfile=/rails/Gemfile --path=/rails/rails/bundle --without mysql

ENV DBTYPE=postgresql

EXPOSE 8080

CMD DB_HOST=tfb-database bundle exec puma -C config/mri_puma.rb -b tcp://0.0.0.0:8080 -e production_postgresql
