FROM ruby:2.4

ADD ./ /rack-sequel

WORKDIR /rack-sequel

RUN bundle install --jobs=4 --gemfile=/rack-sequel/Gemfile --path=/rack-sequel/rack-sequel/bundle

ENV DBTYPE=mysql
CMD bundle exec unicorn -c config/mri_unicorn.rb -o 0.0.0.0 -p 8080 -E production
