FROM ruby:2.4

ADD ./ /roda-sequel
WORKDIR /roda-sequel

RUN bundle install --jobs=4 --gemfile=/roda-sequel/Gemfile --path=/roda-sequel/roda-sequel/bundle

ENV DBTYPE=postgresql
CMD bundle exec unicorn -c config/mri_unicorn.rb -o 0.0.0.0 -p 8080 -E production
