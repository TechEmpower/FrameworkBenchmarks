FROM ruby:2.4

ADD ./ /roda-sequel
WORKDIR /roda-sequel

RUN bundle install --jobs=4 --gemfile=/roda-sequel/Gemfile --path=/roda-sequel/roda-sequel/bundle

ENV DBTYPE=postgresql
CMD bundle exec puma -C config/mri_puma.rb -b tcp://0.0.0.0:8080 -e production
