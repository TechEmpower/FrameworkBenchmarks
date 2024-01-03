FROM ruby:3.3

ENV RUBY_YJIT_ENABLE=1

ADD ./ /sinatra-sequel
WORKDIR /sinatra-sequel

RUN bundle install --jobs=4 --gemfile=/sinatra-sequel/Gemfile --path=/sinatra-sequel/sinatra-sequel/bundle

ENV DBTYPE=mysql

EXPOSE 8080

CMD bundle exec puma -C config/mri_puma.rb -b tcp://0.0.0.0:8080 -e production
