FROM ruby:2.4

ADD ./ /sinatra-sequel
WORKDIR /sinatra-sequel

RUN bundle install --jobs=4 --gemfile=/sinatra-sequel/Gemfile --path=/sinatra-sequel/sinatra-sequel/bundle

ENV DBTYPE=postgresql

EXPOSE 8080

CMD bundle exec unicorn -c config/mri_unicorn.rb -o 0.0.0.0 -p 8080 -E production
