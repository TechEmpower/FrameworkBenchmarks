FROM ruby:2.4

ADD ./ /rack-sequel

WORKDIR /rack-sequel

RUN bundle install --jobs=4 --gemfile=/rack-sequel/Gemfile --path=/rack-sequel/rack-sequel/bundle

ENV DBTYPE=postgresql

EXPOSE 8080

CMD bundle exec puma -C config/mri_puma.rb -b tcp://0.0.0.0:8080 -e production
